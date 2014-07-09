{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (
  main
) where

import Control.Concurrent
import Control.Exception
import Control.Monad

import System.Console.CmdArgs
import System.IO
import System.Posix.Files
import System.Posix.Terminal
import System.Serial

import App

data CmdLineOpts =
  CmdLineOpts { printerPath :: FilePath
              , gcodePath :: FilePath
              , monitor :: Bool
              , verbose :: Bool
              } deriving (Show, Data, Typeable)

cmdLineOpts :: CmdLineOpts
cmdLineOpts =
  CmdLineOpts { printerPath = "/dev/ttyUSB0" &= help "/dev/ttyUSB0"
              , gcodePath = "/dev/null" &= help "path to gcode file"
              , monitor = True &= help "periodically send monitoring commands"
              , verbose = False &= help "verbose output, for debugging"
              } &=
                program "send-gcode" &=
                help "Sends gcode to 3D printer."

main :: IO ()
main = do
  opts <- cmdArgs cmdLineOpts :: IO CmdLineOpts
  gcodeFileStatus <- getFileStatus (gcodePath opts)

  withFile (gcodePath opts) ReadMode $ \gFH ->
    withPrinterFile (printerPath opts) $ \pFH -> do
      gSize <- if (isRegularFile gcodeFileStatus)
                 then hFileSize gFH
                 else return 0
      ps <- (newPrinterState gFH pFH gSize (verbose opts))

      runApp ps $ do
        printFirmwareVersion
        when (not (monitor opts))
          stopGeneratingMonitoringCommands

      rpot <- forkIO $ runApp ps (readPrinterOutput)
      _ <- forkIO $ runApp ps (feedPrinter)
      _ <- forkIO $ runApp ps (feedGcode 0)
      _ <- forkIO $ runApp ps (feedStdin)
      _ <- forkIO $ runApp ps (generateMonitoringCommands)

      catchJust (\e -> case e
                         of UserInterrupt -> Just ()
                            _             -> Nothing)
                (runApp ps (waitForEndOfAllInput))
                (\_ -> runApp ps (executeEmergencyShutdown))

      killThread rpot -- it reads in a loop with large timeout, interrupt if we are at the very end

  return ()

withPrinterFile :: FilePath -> (Handle -> IO a) -> IO a
withPrinterFile pfp act =
  bracket (openPrinterSerialPort pfp) (hClose) act

openPrinterSerialPort :: FilePath -> IO Handle
openPrinterSerialPort pfp = do
  fh <- openSerial pfp B115200 8 One NoParity NoFlowControl
  hSetEcho fh False
  hSetBinaryMode fh True
  --hSetBuffering fh NoBuffering
  hSetBuffering fh $ LineBuffering
  return fh

