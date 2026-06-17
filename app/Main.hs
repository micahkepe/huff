module Main (main) where

import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Lib (compress, decompress)
import Options.Applicative
import System.Directory
import System.Exit (die, exitFailure)
import System.FilePath
import System.IO (hIsTerminalDevice, hPutStrLn, stderr, stdout)

data Output = FileOutput FilePath | StdOut

data Command = Compress FilePath (Maybe FilePath) Bool | Decompress FilePath (Maybe FilePath)

outputParser :: Parser (Maybe FilePath)
outputParser =
    optional
        (strOption (long "output" <> metavar "FILE" <> help "Optional filepath to write to."))

resolveOutput :: Maybe FilePath -> IO Output
resolveOutput (Just fp) = pure (FileOutput fp)
resolveOutput Nothing = do
    isTTY <- hIsTerminalDevice stdout
    if isTTY
        then do
            hPutStrLn stderr "No output file specified and stdout is a terminal. Use -o FILE or pipe output."
            exitFailure
        else pure StdOut

compressParser :: Parser Command
compressParser =
    Compress
        <$> argument str (metavar "FILE" <> help "Input file to compress.")
        <*> outputParser
        <*> switch (short 'f' <> long "force" <> help "Overwrite output path")

decompressParser :: Parser Command
decompressParser =
    Decompress
        <$> argument str (metavar "FILE")
        <*> outputParser

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
        Compress input mOutput overwrite -> do
            content <- TIO.readFile input
            case compress content of
                Nothing -> die "ERROR: failed to compress input"
                Just enc -> do
                    let defaultPath = replaceExtension (takeBaseName input) ".huf"
                    out <- resolveOutput (mOutput <|> Just defaultPath)
                    case out of
                        FileOutput outPath -> do
                            exists <- doesFileExist outPath
                            when
                                (exists && not overwrite)
                                $ do
                                    hPutStrLn stderr "ERROR: File exists. Use -f / --force to force writing to output."
                                    exitFailure

                            BS.writeFile outPath enc
                            when (verbose args) $ do
                                hPutStrLn stderr ("Written to   " ++ show outPath)
                        StdOut -> BS.hPut stdout enc

                    when (verbose args) $ do
                        let encNumBytes = BS.length enc
                        let originalNumBytes = T.length content
                        let ratio = fromIntegral encNumBytes / fromIntegral originalNumBytes :: Double
                        hPutStrLn stderr ("Original:    " ++ show originalNumBytes ++ " bytes")
                        hPutStrLn stderr ("Compressed:  " ++ show encNumBytes ++ " bytes")
                        hPutStrLn stderr ("Ratio:       " ++ show ratio)
        Decompress file out -> do
            content <- BS.readFile file
            case decompress content of
                Just dec -> do
                    case out of
                        Just fp -> writeFile fp dec
                        Nothing -> hPutStrLn stdout dec
                Nothing -> do
                    hPutStrLn stderr "ERROR: failed to decompress input"
                    exitFailure
