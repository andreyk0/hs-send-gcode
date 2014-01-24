{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (
  main
) where

import Control.Concurrent
import Control.Exception
import Control.Monad

import Data.Monoid

import System.Console.CmdArgs
import System.IO
import System.Posix.Files
import System.Posix.Terminal
import System.Serial
import System.Timeout

import Control.Monad.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.SSem (SSem)
import qualified Control.Concurrent.SSem as SSem

import Control.Monad.Trans.Reader
import Control.Monad.IO.Class

import Control.Applicative

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


type App = ReaderT PrinterState IO


main :: IO ()
main = do
  opts <- cmdArgs cmdLineOpts :: IO CmdLineOpts
  gcodeFileStatus <- getFileStatus (gcodePath opts)

  ps <-
    withFile (gcodePath opts) ReadMode $ \gFH ->
      withPrinterFile (printerPath opts) $ \pFH -> do
        newPs <- (newPrinterState gFH pFH)
        gSize <- if (isRegularFile gcodeFileStatus)
                   then hFileSize gFH
                   else return 0
        let ps = newPs { gcodeTotalBytes = gSize }

        runReaderT (writePrinterLine "M115") ps -- print firmware version / capabilities on connect
        _ <- forkIO $ runReaderT (readPrinterOutput) ps
        _ <- forkIO $ runReaderT (feedPrinter) ps
        _ <- forkIO $ runReaderT (feedGcode 0) ps
        _ <- forkIO $ runReaderT (feedStdin) ps

        if (monitor opts)
          then do _ <- forkIO $ runReaderT (generateMonitoringCommands) ps
                  return ()
          else SSem.signal (endOfInputSemaphore ps)

        runReaderT (waitForEndOfAllInput) ps
        return ps

  runReaderT (waitForEndOfAllOutput) ps

newPrinterState :: Handle -- | gcode file handle
                -> Handle -- | printer file handle
                -> IO PrinterState
newPrinterState gFH pFH =
  (PrinterState) <$> (return gFH)
                 <*> (return pFH)
                 <*> (return 0)
                 <*> (atomically $ newTVar 0)
                 <*> (atomically $ newTVar Nothing)
                 <*> (atomically $ newTVar Nothing)
                 <*> (SSem.new 0)
                 <*> (SSem.new 0)
                 <*> (atomically $ newTVar 0)
                 <*> (atomically $ newTVar True)

waitForEndOfAllInput :: App ()
waitForEndOfAllInput = do
  ps <- ask
  debug "waiting for stdin/stdout to exit"
  liftIO $ SSem.waitN (endOfInputSemaphore ps) 2 -- stdin/gcode file
  liftSTM $ writeTVar (shouldGenerateMonitoringCommands ps) False
  debug "waiting for monitor thread to exit"
  liftIO $ SSem.wait (endOfInputSemaphore ps)
  debug "waiting for printer feed thread to exit"
  sendCommand EndOfAllStreams
  liftIO $ SSem.wait (endOfInputSemaphore ps)
  debug "input threads have exited"

waitForEndOfAllOutput :: App ()
waitForEndOfAllOutput = do
  ps <- ask
  debug "waiting for printer reader to exit"
  liftIO $ SSem.wait (endOfOutputSemaphore ps)
  debug "all done"

maxNumUnackLines :: Int
maxNumUnackLines = 2

makeGcodeLine :: Text -> Maybe PrinterCmd
makeGcodeLine l =
  let normalizedTxt = T.strip $ T.takeWhile (';' /=) l
   in if (T.null normalizedTxt)
      then Nothing
      else Just $ GCodeLine normalizedTxt

sendCommand :: PrinterCmd -> App ()
sendCommand pcmd = do
  ps <- ask
  liftSTM $ do
    bf <- readTVar (sendLineBufferFill ps)
    pf <- readTVar (printerFeed ps)
    if (bf < maxNumUnackLines)
      then case pf
             of Nothing -> do modifyTVar' (sendLineBufferFill ps) (+1)
                              writeTVar (printerFeed ps) (Just pcmd)
                Just _ -> retry
      else retry

sendPriorityCommand :: PrinterCmd -> App ()
sendPriorityCommand pcmd = do
  ps <- ask
  liftSTM $ do
    ppf <- readTVar (printerPriorityFeed ps)
    case ppf
      of Nothing -> do modifyTVar' (sendLineBufferFill ps) (+1)
                       writeTVar (printerPriorityFeed ps) (Just pcmd)
         Just _ -> retry

feedStdin :: App ()
feedStdin = do
  isOpen <- liftIO $ hIsOpen stdin
  isEof <- liftIO $ hIsEOF stdin
  if (isOpen && (not isEof))
    then do line <- liftIO $ TIO.hGetLine stdin
            case makeGcodeLine line
              of Just gcl -> sendPriorityCommand gcl
                 Nothing -> return ()
            feedStdin
    else sendCommand EndOfStream

feedGcode :: Int -> App ()
feedGcode lineCounter = do
  ps <- ask
  isOpen <- liftIO $ hIsOpen (gcodeFH ps)
  isEof <- liftIO $ hIsEOF (gcodeFH ps)

  if (isOpen && (not isEof))
    then do line <- liftIO $ TIO.hGetLine (gcodeFH ps)
            liftSTM $ modifyTVar (gcodeSentBytes ps) (+ (T.length line))
            when (lineCounter `mod` 100 == 0) $
              progress
            case makeGcodeLine line
              of Just gcl -> sendCommand gcl
                 Nothing -> return ()
            feedGcode (lineCounter +1)
    else sendCommand EndOfStream

generateMonitoringCommands :: App ()
generateMonitoringCommands = do
  ps <- ask
  ssmc <- liftSTM $ readTVar (shouldGenerateMonitoringCommands ps)
  if (ssmc)
    then do sendCommand $ GCodeLine "M105" -- 'normal' commands, marlin prints progress when it waits for temperature
            sendCommand $ GCodeLine "M114"
            liftIO $ threadDelay 5000000
            generateMonitoringCommands
    else sendCommand EndOfStream

feedPrinter :: App ()
feedPrinter = do
  ps <- ask
  cmd <- liftSTM $ do
           ppf <- readTVar (printerPriorityFeed ps)
           pf <- readTVar (printerFeed ps)
           case ppf
             of Just c -> do writeTVar (printerPriorityFeed ps) Nothing
                             return c
                Nothing -> case pf
                             of Just c -> do writeTVar (printerFeed ps) Nothing
                                             return c
                                Nothing -> retry

  debug $ "feedPrinter(" <> (T.pack . show) cmd <> ")"

  case cmd
    of EndOfStream -> do sv <- liftIO $ do SSem.signal (endOfInputSemaphore ps)
                                           SSem.getValue (endOfInputSemaphore ps)
                         debug $ "EOS: " <> (T.pack .show) sv
                         feedPrinter

       EndOfAllStreams -> liftIO $ SSem.signal (endOfInputSemaphore ps)

       GCodeLine l -> do writePrinterLine l
                         feedPrinter

readPrinterOutput :: App ()
readPrinterOutput = do
  ps <- ask
  isOpen <- liftIO $ hIsOpen (printerFH ps)
  if (isOpen)
   then do maybeLine <- liftIO $ timeout (1000000) $ TIO.hGetLine (printerFH ps)
           case maybeLine
             of Just line -> do liftIO $ when (T.isPrefixOf "ok" line) $ do
                                  atomically $ modifyTVar' (sendLineBufferFill ps) (\i -> i-1)
                                printerResponse line
                                readPrinterOutput
                Nothing -> readPrinterOutput -- check conditions again
   else liftIO $ SSem.signal (endOfOutputSemaphore ps)


writePrinterLine :: Text -> App ()
writePrinterLine l = do
  ps <- ask
  liftIO $ do
    TIO.hPutStrLn (printerFH ps) l
    hFlush (printerFH ps)

withPrinterFile :: FilePath -> (Handle -> IO a) -> IO a
withPrinterFile pfp act = do
  bracket (openPrinterSerialPort pfp) (hClose) act

openPrinterSerialPort :: FilePath -> IO Handle
openPrinterSerialPort pfp = do
  fh <- openSerial pfp B115200 8 One NoParity NoFlowControl
  hSetEcho fh False
  hSetBinaryMode fh True
  return fh

liftSTM :: STM a -> App a
liftSTM = liftIO . atomically

debug :: Text -> App ()
debug t = liftIO $ TIO.hPutStrLn stdout $ "DEBUG: " <> t

progress :: App ()
progress = do
  ps <- ask
  let tb = fromIntegral (gcodeTotalBytes ps) :: Double
  sb <- liftSTM $ readTVar (gcodeSentBytes ps) >>= return . fromIntegral
  let pct = (sb / (1 + tb)) * 100.0
      pctTxt = TL.toStrict . TLB.toLazyText $ TF.fixed 2 pct
  liftIO $ TIO.hPutStrLn stdout $ "PROGRESS: " <> pctTxt

printerResponse :: Text -> App ()
printerResponse t = liftIO $ when (t /= "ok") $ do -- print all but trivial responses
  case T.stripPrefix "ok " t
    of Just x  -> TIO.putStrLn $ "PRINTER: " <> x
       Nothing -> TIO.putStrLn $ "PRINTER: " <> t
