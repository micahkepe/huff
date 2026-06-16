module Main (main) where

import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Lib (compress, decompress)
import Options.Applicative
import System.Directory
import System.Exit (exitFailure)
import System.FilePath
import System.IO

data Command = Compress FilePath (Maybe FilePath) Bool | Decompress FilePath

compressParser :: Parser Command
compressParser =
    Compress
        <$> argument str (metavar "FILE" <> help "Input file to compress.")
        <*> optional
            (strOption (long "output" <> metavar "FILE" <> help "Optional filepath to write to. Defaults to current directory."))
        <*> switch (short 'f' <> long "force" <> help "Overwrite output path")

decompressParser :: Parser Command
decompressParser =
    Decompress
        <$> argument str (metavar "FILE")

commandParser :: Parser Command
commandParser =
    subparser
        ( command "compress" (info (compressParser <**> helper) (progDesc "Compress a file."))
            <> (command "decompress" (info (decompressParser <**> helper) (progDesc "Decompress a Huffman-encoded file")))
        )

data Args = Args
    { cmd :: Command
    , verbose :: Bool
    }

argsParser :: Parser Args
argsParser =
    Args
        <$> commandParser
        <*> switch (short 'v' <> long "verbose" <> help "Verbose output")

main :: IO ()
main = do
    args <- execParser (info (argsParser <**> helper) (progDesc "Simple Huffman encoder."))
    case cmd args of
        Compress input output overwrite -> do
            content <- TIO.readFile input
            case compress content of
                Just enc -> do
                    let outPath = case output of
                            Just f -> f
                            Nothing -> replaceExtension (takeBaseName input) ".huf"
                    exists <- doesFileExist outPath
                    when
                        (exists && not overwrite)
                        $ do
                            hPutStrLn stderr "ERROR: File exists. Use -f / --force to force writing to output."
                            exitFailure
                    BS.writeFile outPath enc
                    let encNumBytes = BS.length enc
                    let originalNumBytes = T.length content
                    let ratio = fromIntegral encNumBytes / fromIntegral originalNumBytes :: Double
                    when (verbose args) $ do
                        hPutStrLn stderr ("Original:    " ++ show originalNumBytes ++ " bytes")
                        hPutStrLn stderr ("Compressed:  " ++ show encNumBytes ++ " bytes")
                        hPutStrLn stderr ("Ratio:       " ++ show ratio)
                        hPutStrLn stderr ("Written to   " ++ show outPath)
                Nothing -> error "ERROR: failed to compress input"
        Decompress file -> do
            content <- BS.readFile file
            case decompress content of
                Just dec -> do
                    hPutStrLn stdout dec
                Nothing -> do
                    hPutStrLn stderr "ERROR: failed to decompress input"
                    exitFailure
