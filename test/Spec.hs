{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Data.Text (Text)
import qualified Data.Text as T
import Lib (compress, decompress)
import Test.QuickCheck

-- helpers

roundtrip :: Text -> Maybe Bool
roundtrip input = do
    compressed <- compress input
    dec <- decompress compressed
    Just (T.unpack input == dec)

-- prop tests

prop_roundtrip :: String -> Bool
prop_roundtrip s =
    let input = T.pack s
     in if T.null input
            then roundtrip input == Nothing
            else roundtrip input == Just True

prop_singleChar :: Char -> Bool
prop_singleChar c = roundtrip (T.singleton c) == Just True

return []

main :: IO ()
main = do True <- $quickCheckAll; return ()
