module Main where

import GitHub.Hook

import Data.Aeson (decode)
import Data.ByteString.Lazy.Char8 (pack)
import Data.Maybe

import System.Exit
import System.Directory


parsePayload :: FilePath -> IO Bool
parsePayload "."  = return True
parsePayload ".." = return True
parsePayload file = do
    contents <- readFile $ "./tests/fixtures/" ++ file
    case (decode (pack contents) :: Maybe Payload) of
        Nothing   -> putStrLn ("Failed to parse " ++ file) >> return False
        otherwise -> return True


main :: IO ()
main = do
    files <- getDirectoryContents "./tests/fixtures/"
    results <- mapM parsePayload files

    if and results
        then exitWith ExitSuccess
        else exitWith (ExitFailure 1)
