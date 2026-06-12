module Main (main) where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
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
            content <- TIO.readFile fd
            case compress content of
                Just enc -> do
                    let encNumBytes = BS.length enc
                    let originalNumBytes = T.length content
                    let ratio = fromIntegral encNumBytes / fromIntegral originalNumBytes :: Double
                    hPutStrLn stdout ("Original:    " ++ show originalNumBytes ++ " bytes")
                    hPutStrLn stdout ("Compressed:  " ++ show encNumBytes ++ " bytes")
                    hPutStrLn stdout ("Ratio:       " ++ show ratio)
                    hPutStrLn stdout "---"
                    case roundtrip content of
                        Just True -> hPutStrLn stdout "Match!"
                        _ -> error "roundtrip failed"
                Nothing -> error "failed to compress input"
        _ -> do
            hPutStrLn stderr "Error: incorrect number of args"
            usage
