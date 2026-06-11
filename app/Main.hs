module Main (main) where

import Lib (compress, roundtrip)
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
            case compress content of
                Just enc -> do
                    let encNumBits = length enc
                    let originalNumBits = length content * 8
                    let ratio = fromIntegral encNumBits / fromIntegral originalNumBits :: Double
                    hPutStrLn stdout ("Original:    " ++ show originalNumBits ++ " bits")
                    hPutStrLn stdout ("Compressed:  " ++ show encNumBits ++ " bits")
                    hPutStrLn stdout ("Ratio:       " ++ show ratio)
                    hPutStrLn stdout "---"
                    case roundtrip content of
                        Just True -> hPutStrLn stdout "Match!"
                        _ -> error "roundtrip failed"
                Nothing -> error "nothing to see here"
        _ -> do
            hPutStrLn stderr "Error: incorrect number of args"
            usage
