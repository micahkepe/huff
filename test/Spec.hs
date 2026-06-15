-- import Lib (compress, roundtrip)
--
-- roundtrip :: Text -> Maybe Bool
-- roundtrip input = do
--     compressed <- compress input
--     dec <- decompress compressed
--     Just (T.unpack input == dec)

main :: IO ()
main = putStrLn "Test suite not yet implemented"
