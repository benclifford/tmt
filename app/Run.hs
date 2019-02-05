{-# OPTIONS_GHC -Wall -Werror #-}

-- | Various ways of running command lines
module Run where

import Data.List (intersperse)
import qualified System.Exit as Exit
import qualified System.Process as Process

run :: String -> IO ()
run command = do
  putStrLn $ "+ " ++ command 
  Process.callCommand command

runArgs :: [String] -> IO ()
runArgs commands = do
  putStrLn $ "+ " ++ (concat $ intersperse " // " commands)
  Process.callProcess (head commands) (tail commands)

runRead :: String -> IO String
runRead command = do
  putStrLn $ "+ " ++ command
  Process.readCreateProcess (Process.shell command) ""

runReadRet :: String -> IO (Exit.ExitCode, String, String)
runReadRet command = do
  putStrLn $ "+ " ++ command
  Process.readCreateProcessWithExitCode (Process.shell command) ""

