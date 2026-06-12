module Lib (compress, buildTree, buildCodeTable, encode, decode, roundtrip) where

import Data.Bits (Bits (testBit), setBit)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.List (insert, sort)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T

import Data.Word (Word8)

compress :: Text -> Maybe ByteString
compress input = do
    tree <- buildTree input
    let ct = buildCodeTable tree
    encode input ct

roundtrip :: Text -> Maybe Bool
roundtrip input = do
    tree <- buildTree input
    let ct = buildCodeTable tree
    enc <- encode input ct
    dec <- decode enc tree
    Just ((T.unpack input) == dec)

data HuffTree
    = Node Int HuffTree HuffTree
    | Leaf Char Int
    deriving (Show, Read, Eq)

freq :: Text -> Map Char Int
freq str = Map.fromListWith (+) (map (\c -> (c, 1)) (T.unpack str))

toLeaves :: Map Char Int -> [HuffTree]
toLeaves freqMap =
    map (\(c, f) -> Leaf c f) (Map.toList freqMap)

weight :: HuffTree -> Int
weight (Node w _ _) = w
weight (Leaf _ w) = w

instance Ord HuffTree where
    compare a b = compare (weight a) (weight b)

merge :: [HuffTree] -> Maybe HuffTree
merge [] = Nothing
merge [t] = Just t
merge (a : b : rest) =
    let combined = Node ((weight a) + (weight b)) a b
        rest' = insert combined rest
     in merge rest'

buildTree :: Text -> Maybe HuffTree
buildTree input =
    let freqMap = freq input
        nodes = toLeaves freqMap
        sorted = sort nodes
     in merge sorted

buildCodeTable :: HuffTree -> Map Char [Bool]
buildCodeTable tree = go [] tree
  where
    go path (Leaf c _) = Map.singleton c path
    go path (Node _ left right) =
        let leftTree = go (path ++ [False]) left
            rightTree = go (path ++ [True]) right
         in Map.union leftTree rightTree

{- | Packs the input Boolean array into a ByteString with the following scheme:

  ```
  [padding, byte_0, byte_1, ...]
  ```

  where padding = [0-7]

For decoding:
1.  First read the padding byte to know how many bits to lob off at the end of
    the ByteString.
2.  Read each subsequent byte (minus the padding bits on the last byte), walking
    the Huffman tree to find the corresponding Char.
-}
packBits :: [Bool] -> ByteString
packBits enc =
    let padCount = (8 - (length enc `mod` 8)) `mod` 8
        bytes = go enc 0 0
     in BS.pack (fromIntegral padCount : bytes)
  where
    go [] _ 0 = [] -- no partial byte remaining
    go [] byte _ = [byte] -- flush partial byte
    go (curr : rest) byte idx
        | idx == 8 = byte : go (curr : rest) 0 0 -- just finished the current byte, emit it and continue
        | curr = go rest (setBit byte (7 - idx)) (idx + 1) -- current bit is True
        | otherwise = go rest byte (idx + 1) -- current bit is False, advance to next idx

byteToBits :: Word8 -> [Bool]
byteToBits byte = [testBit byte (7 - i) | i <- [0 .. 7]]

unpackBits :: ByteString -> [Bool]
unpackBits str = go (BS.unpack str)
  where
    go [] = []
    -- first byte in input (padding)
    go (pad : rest) =
        let allBits = concatMap byteToBits rest
            padCount = fromIntegral pad
         in take (length allBits - padCount) allBits

encode :: Text -> Map Char [Bool] -> Maybe ByteString
encode input tbl =
    let res = map (\c -> Map.lookup c tbl) (T.unpack input)
     in fmap packBits (fmap concat (sequence res))

decode :: ByteString -> HuffTree -> Maybe String
decode str root =
    go (unpackBits str) root
  where
    -- done decoding
    go [] (Leaf c _) = Just [c]
    go [] (Node _ _ _) = Nothing
    -- reached leaf, emit char and reset root
    go bits' (Leaf c _) = fmap (c :) (go bits' root)
    -- at node:
    -- \* if False -> go left
    -- \* if True -> go right
    -- (NOTE: opposite of HuffTree construction)
    go (False : rest) (Node _ left _) = go rest left
    go (True : rest) (Node _ _ right) = go rest right
