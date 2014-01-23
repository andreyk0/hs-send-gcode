{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (
  main
) where

import Control.Exception
import System.Console.CmdArgs
import System.IO
import System.Posix.Terminal
import System.Serial
import Data.Monoid
import Control.Concurrent
import System.Posix.Files
import System.Timeout
import Control.Monad

import Control.Monad.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.SSem (SSem)
import qualified Control.Concurrent.SSem as SSem

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Format as TF
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Text.IO as TIO

data CmdLineOpts =
  CmdLineOpts { printerPath :: FilePath
              , gcodePath :: FilePath
              , monitor :: Bool
              } deriving (Show, Data, Typeable)

cmdLineOpts :: CmdLineOpts
cmdLineOpts =
  CmdLineOpts { printerPath = "/dev/ttyUSB0" &= help "/dev/ttyUSB0"
              , gcodePath = "/dev/null" &= help "path to gcode file"
              , monitor = True &= help "periodically send monitoring commands"
              } &=
                program "send-gcode" &=
                help "Sends gcode to 3D printer."

data PrinterCmd = GCodeLine Text
                | EndOfStream
                | EndOfAllStreams
                deriving (Eq,Show)

data PrinterState =
  PrinterState { gcodeFH :: Handle
               , printerFH :: Handle
               , gcodeTotalBytes :: Integer
               , gcodeSentBytes :: TVar Int
               , printerFeed :: TVar (Maybe PrinterCmd)
               , printerPriorityFeed :: TVar (Maybe PrinterCmd)
               , endOfInputSemaphore :: SSem -- we count it up every time we reach an end of stream
               , endOfOutputSemaphore :: SSem -- to signal exit from printer reader thread
               , sendLineBufferFill :: TVar Int -- this is to throttle how fast we feed gcode from the file
               , shouldGenerateMonitoringCommands :: TVar Bool
               }

main :: IO ()
main = do
  opts <- cmdArgs cmdLineOpts :: IO CmdLineOpts
  gcodeFileStatus <- getFileStatus (gcodePath opts)
  endOfInputSem <- SSem.new 0
  printerReaderExitSem <- SSem.new 0

  withFile (gcodePath opts) ReadMode $ \gFH ->
    withPrinterFile (printerPath opts) $ \pFH -> do
      TIO.hPutStrLn pFH "M115" -- print firmware version / capabilities on connect
      slBF <- atomically $ newTVar 0
      gSize <- if (isRegularFile gcodeFileStatus)
                 then hFileSize gFH
                 else return 0
      sentBytes <- atomically $ newTVar 0
      pFeed <- atomically $ newTVar Nothing
      ppFeed <- atomically $ newTVar Nothing
      ssmc <- atomically $ newTVar True
      let pState = PrinterState { gcodeFH = gFH
                                , printerFH = pFH
                                , gcodeTotalBytes = (gSize)
                                , gcodeSentBytes = sentBytes
                                , printerFeed = pFeed
                                , printerPriorityFeed = ppFeed
                                , endOfInputSemaphore = endOfInputSem
                                , endOfOutputSemaphore = printerReaderExitSem
                                , sendLineBufferFill = slBF
                                , shouldGenerateMonitoringCommands = ssmc
                                }
      _ <- forkIO $ readPrinterOutput pState
      _ <- forkIO $ feedPrinter pState
      _ <- forkIO $ feedGcode pState 0
      _ <- forkIO $ feedStdin pState

      if (monitor opts)
        then do _ <- forkIO $ generateMonitoringCommands pState
                return ()
        else SSem.signal endOfInputSem

      debug "waiting for stdin/stdout to exit"
      SSem.waitN endOfInputSem 2 -- stdin/gcode file
      atomically $ writeTVar ssmc False
      debug "waiting for monitor thread to exit"
      SSem.wait endOfInputSem
      debug "waiting for printer feed thread to exit"
      sendCommand pState EndOfAllStreams
      SSem.wait endOfInputSem
      debug "input threads have exited"

  debug "waiting for printer reader to exit"
  SSem.wait printerReaderExitSem
  debug "all done"

maxNumUnackLines :: Int
maxNumUnackLines = 2

makeGcodeLine :: Text -> Maybe PrinterCmd
makeGcodeLine l =
  let normalizedTxt = T.strip $ T.takeWhile (';' /=) l
   in if (T.null normalizedTxt)
      then Nothing
      else Just $ GCodeLine normalizedTxt

sendCommand :: PrinterState -> PrinterCmd -> IO ()
sendCommand ps pcmd =
  atomically $ do
    bf <- readTVar (sendLineBufferFill ps)
    pf <- readTVar (printerFeed ps)
    if (bf < maxNumUnackLines)
      then case pf
             of Nothing -> do modifyTVar' (sendLineBufferFill ps) (+1)
                              writeTVar (printerFeed ps) (Just pcmd)
                Just _ -> retry
      else retry

sendPriorityCommand :: PrinterState -> PrinterCmd -> IO ()
sendPriorityCommand ps pcmd = atomically $ do
  ppf <- readTVar (printerPriorityFeed ps)
  case ppf
    of Nothing -> do modifyTVar' (sendLineBufferFill ps) (+1)
                     writeTVar (printerPriorityFeed ps) (Just pcmd)
       Just _ -> retry

feedStdin :: PrinterState -> IO ()
feedStdin pState = do
  isOpen <- hIsOpen stdin
  isEof <- hIsEOF stdin
  if (isOpen && (not isEof))
    then do line <- TIO.hGetLine stdin
            case makeGcodeLine line
              of Just gcl -> sendPriorityCommand pState gcl
                 Nothing -> return ()
            feedStdin pState
    else sendCommand pState EndOfStream

feedGcode :: PrinterState -> Int -> IO ()
feedGcode pState lineCounter = do
  isOpen <- hIsOpen (gcodeFH pState)
  isEof <- hIsEOF (gcodeFH pState)

  if (isOpen && (not isEof))
    then do line <- TIO.hGetLine (gcodeFH pState)
            atomically $ modifyTVar (gcodeSentBytes pState) (+ (T.length line))
            when (lineCounter `mod` 100 == 0) $
              progress pState
            case makeGcodeLine line
              of Just gcl -> sendCommand pState gcl
                 Nothing -> return ()
            feedGcode pState (lineCounter +1)
    else sendCommand pState EndOfStream

generateMonitoringCommands :: PrinterState -> IO ()
generateMonitoringCommands pState = do
  ssmc <- atomically $ readTVar (shouldGenerateMonitoringCommands pState)
  if (ssmc)
    then do sendCommand pState $ GCodeLine "M105" -- 'normal' commands, marlin prints progress when it waits for temperature
            sendCommand pState $ GCodeLine "M114"
            threadDelay 5000000
            generateMonitoringCommands pState
    else sendCommand pState EndOfStream

feedPrinter :: PrinterState -> IO ()
feedPrinter pState = do
  cmd <- atomically $ do
           ppf <- readTVar (printerPriorityFeed pState)
           pf <- readTVar (printerFeed pState)
           case ppf
             of Just c -> do writeTVar (printerPriorityFeed pState) Nothing
                             return c
                Nothing -> case pf
                             of Just c -> do writeTVar (printerFeed pState) Nothing
                                             return c
                                Nothing -> retry

  debug $ "feedPrinter(" <> (T.pack . show) cmd <> ")"
  case cmd
    of EndOfStream -> do SSem.signal (endOfInputSemaphore pState)
                         sv <- SSem.getValue (endOfInputSemaphore pState)
                         debug $ "EOS: " <> (T.pack .show) sv
                         feedPrinter pState

       EndOfAllStreams -> SSem.signal (endOfInputSemaphore pState)

       GCodeLine l -> do TIO.hPutStrLn (printerFH pState) l
                         hFlush (printerFH pState)
                         feedPrinter pState

readPrinterOutput :: PrinterState -> IO ()
readPrinterOutput pState = do
  isOpen <- hIsOpen (printerFH pState)
  if (isOpen)
   then do maybeLine <- timeout (1000000) $ TIO.hGetLine (printerFH pState)
           case maybeLine
             of Just line -> do when (T.isPrefixOf "ok" line) $ do
                                  atomically $ modifyTVar' (sendLineBufferFill pState) (\i -> i-1)
                                printerResponse line
                                readPrinterOutput pState
                Nothing -> readPrinterOutput pState -- check conditions again
   else SSem.signal (endOfOutputSemaphore pState)

withPrinterFile :: FilePath -> (Handle -> IO a) -> IO a
withPrinterFile pfp act = do
  bracket (openPrinterSerialPort pfp) (hClose) act

openPrinterSerialPort :: FilePath -> IO Handle
openPrinterSerialPort pfp = do
  fh <- openSerial pfp B115200 8 One NoParity NoFlowControl
  hSetEcho fh False
  hSetBinaryMode fh True
  return fh

debug :: Text -> IO ()
debug t = TIO.hPutStrLn stdout $ "DEBUG: " <> t

progress :: PrinterState -> IO ()
progress pState = do
  let tb = fromIntegral (gcodeTotalBytes pState) :: Double
  sb <- atomically $ readTVar (gcodeSentBytes pState) >>= return . fromIntegral :: IO Double
  let pct = (sb / (1 + tb)) * 100.0
      pctTxt = TL.toStrict . TLB.toLazyText $ TF.fixed 2 pct
  TIO.hPutStrLn stdout $ "PROGRESS: " <> pctTxt

printerResponse :: Text -> IO ()
printerResponse t = when (t /= "ok") $ do -- print all but trivial responses
  case T.stripPrefix "ok " t
    of Just x  -> TIO.putStrLn $ "PRINTER: " <> x
       Nothing -> TIO.putStrLn $ "PRINTER: " <> t
