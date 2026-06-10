module Main (main) where

import Lib (buildTree)
import System.Environment
import System.IO

usage :: IO ()
usage = do
    prog <- getProgName
    hPutStrLn stderr (prog ++ " <FILE>")

main :: IO ()
main = do
    args <- getArgs
    case args of
        [fd] -> do
            content <- readFile fd
            case buildTree content of
                Just tree -> print tree
                Nothing -> error "nothing to see here"
        _ -> do
            hPutStrLn stderr "Error: incorrect number of args"
            usage
