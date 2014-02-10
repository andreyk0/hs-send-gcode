{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module App (
  App
  , newPrinterState
  , runApp
  , executeEmergencyShutdown
  , printFirmwareVersion
  , readPrinterOutput
  , feedPrinter
  , feedGcode
  , feedStdin
  , generateMonitoringCommands
  , stopGeneratingMonitoringCommands
  , waitForEndOfAllInput
  , waitForEndOfAllOutput
) where

import Control.Concurrent
import Control.Monad

import Data.Monoid

import System.IO
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

import qualified GCodeDictionary as GCD


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
               , verbose :: Bool -- turns on debug log
               }


newtype App a = SGApp { runSGApp :: ReaderT PrinterState IO a
                      } deriving (Monad, MonadIO)

getPrinterState :: App PrinterState
getPrinterState = SGApp (ask)

runApp :: PrinterState -> App a -> IO a
runApp ps a = runReaderT (runSGApp a) ps

newPrinterState :: Handle -- | gcode file handle
                -> Handle -- | printer file handle
                -> Integer -- | gcode file size
                -> Bool -- | verbose output
                -> IO PrinterState
newPrinterState gFH pFH gSize verb =
  (PrinterState) <$> (return gFH)
                 <*> (return pFH)
                 <*> (return gSize)
                 <*> (atomically $ newTVar 0)
                 <*> (atomically $ newTVar Nothing)
                 <*> (atomically $ newTVar Nothing)
                 <*> (SSem.new 0)
                 <*> (SSem.new 0)
                 <*> (atomically $ newTVar 0)
                 <*> (atomically $ newTVar True)
                 <*> (return verb)

waitForEndOfAllInput :: App ()
waitForEndOfAllInput = do
  ps <- getPrinterState
  logDebug "waiting for stdin/stdout to exit"
  liftIO $ SSem.waitN (endOfInputSemaphore ps) 2 -- stdin/gcode file
  stopGeneratingMonitoringCommands
  logDebug "waiting for monitor thread to exit"
  liftIO $ SSem.wait (endOfInputSemaphore ps)
  logDebug "waiting for printer feed thread to exit"
  sendCommand EndOfAllStreams
  liftIO $ SSem.wait (endOfInputSemaphore ps)
  logDebug "input threads have exited"

waitForEndOfAllOutput :: App ()
waitForEndOfAllOutput = do
  ps <- getPrinterState
  logDebug "waiting for printer reader to exit"
  liftIO $ SSem.wait (endOfOutputSemaphore ps)
  logDebug "all done"

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
  ps <- getPrinterState
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
  ps <- getPrinterState
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
  ps <- getPrinterState
  isOpen <- liftIO $ hIsOpen (gcodeFH ps)
  isEof <- liftIO $ hIsEOF (gcodeFH ps)

  if (isOpen && (not isEof))
    then do line <- liftIO $ TIO.hGetLine (gcodeFH ps)
            liftSTM $ modifyTVar (gcodeSentBytes ps) (+ (T.length line))
            when (lineCounter `mod` 100 == 0) $
              logProgress
            case makeGcodeLine line
              of Just gcl -> sendCommand gcl
                 Nothing -> return ()
            feedGcode (lineCounter +1)
    else sendCommand EndOfStream

generateMonitoringCommands :: App ()
generateMonitoringCommands = do
  ps <- getPrinterState
  ssmc <- liftSTM $ readTVar (shouldGenerateMonitoringCommands ps)
  if (ssmc)
    then do forM_ [ GCD.getExtruderTemperature
                  , GCD.getCurrentPosition
                  ] (sendCommand . GCodeLine)
            liftIO $ threadDelay 5000000
            generateMonitoringCommands
    else sendCommand EndOfStream

stopGeneratingMonitoringCommands :: App ()
stopGeneratingMonitoringCommands = do
  ps <- getPrinterState
  liftSTM $ writeTVar (shouldGenerateMonitoringCommands ps) False

feedPrinter :: App ()
feedPrinter = do
  ps <- getPrinterState
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

  logDebug $ "feedPrinter(" <> (T.pack . show) cmd <> ")"

  case cmd
    of EndOfStream -> do sv <- liftIO $ do SSem.signal (endOfInputSemaphore ps)
                                           SSem.getValue (endOfInputSemaphore ps)
                         logDebug $ "EOS: " <> (T.pack .show) sv
                         feedPrinter

       EndOfAllStreams -> liftIO $ SSem.signal (endOfInputSemaphore ps)

       GCodeLine l -> do writePrinterLine l
                         feedPrinter

  where writePrinterLine :: Text -> App ()
        writePrinterLine l = do
          ps <- getPrinterState
          liftIO $ do
            TIO.hPutStrLn (printerFH ps) l
            hFlush (printerFH ps)


readPrinterOutput :: App ()
readPrinterOutput = do
  ps <- getPrinterState
  isOpen <- liftIO $ hIsOpen (printerFH ps)
  if (isOpen)
   then do maybeLine <- liftIO $ timeout (1000000) $ TIO.hGetLine (printerFH ps)
           case maybeLine
             of Just line -> do when (T.isPrefixOf "ok" line) $
                                  liftSTM $ modifyTVar' (sendLineBufferFill ps) (\i -> i-1)
                                logPrinterResponse line
                                readPrinterOutput
                Nothing -> readPrinterOutput -- check conditions again
   else liftIO $ SSem.signal (endOfOutputSemaphore ps)

-- print firmware version / capabilities on connect
printFirmwareVersion :: App ()
printFirmwareVersion = sendCommand $ GCodeLine GCD.firmwareVersion

executeEmergencyShutdown :: App ()
executeEmergencyShutdown = do
  ps <- getPrinterState
  liftSTM $ modifyTVar' (sendLineBufferFill ps) (+ (maxNumUnackLines+1)) -- block all remaining normal input while process is stopping
  forM_ [ GCD.extruderTemperatureOff
        , GCD.bedTemperatureOff
        , GCD.fanOff
        , GCD.disableMotors
        , GCD.emergencyStop
        ] (sendPriorityCommand . GCodeLine)

liftSTM :: STM a -> App a
liftSTM = liftIO . atomically

logDebug :: Text -> App ()
logDebug t = do
  ps <- getPrinterState
  when (verbose ps) $
    liftIO $ TIO.hPutStrLn stdout $ "DEBUG: " <> t

logProgress :: App ()
logProgress = do
  ps <- getPrinterState
  sb <- liftSTM $ readTVar (gcodeSentBytes ps) >>= return . fromIntegral
  let tb = if (gcodeTotalBytes ps) < 1
             then maxBound :: Int -- this'll keep it at 0 when streaming
             else fromIntegral $ (gcodeTotalBytes ps)
      tbd = fromIntegral tb :: Double
      pct = (sb / tbd) * 100.0
      pctTxt = TL.toStrict . TLB.toLazyText $ TF.fixed 2 pct

  liftIO $ TIO.hPutStrLn stdout $ "PROGRESS: " <> pctTxt

logPrinterResponse :: Text -> App ()
logPrinterResponse t = liftIO $ when (t /= "ok") $ do -- print all but trivial responses
  case T.stripPrefix "ok " t
    of Just x  -> TIO.putStrLn $ "PRINTER: " <> x
       Nothing -> TIO.putStrLn $ "PRINTER: " <> t
