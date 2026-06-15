module Lib (compress, decompress) where

import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Char (chr, ord)
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
    let ser_tree = serializeTree tree
    enc <- encode input ct
    Just (BS.append ser_tree enc)

decompress :: ByteString -> Maybe String
decompress str = do
    (tree, bs) <- deserializeTree str
    let padCount = fromIntegral (BS.index bs 0)
        dataBytes = BS.drop 1 bs
        totalBits = BS.length dataBytes * 8 - padCount
        -- go node byteIdx bitIdx
        go :: HuffTree -> Int -> Int -> Maybe String
        go (Leaf c _) byteIdx bitIdx
            | byteIdx * 8 + bitIdx >= totalBits = Just [c] -- last char
            | otherwise = fmap (c :) (go tree byteIdx bitIdx)
        go (Node _ left right) byteIdx bitIdx
            | byteIdx * 8 + bitIdx >= totalBits = Nothing -- should not end up here
            | testBit (BS.index dataBytes byteIdx) (7 - bitIdx) = go right nextByte nextBit
            | otherwise = go left nextByte nextBit
          where
            nextBit = (bitIdx + 1) `mod` 8
            nextByte = if bitIdx == 7 then byteIdx + 1 else byteIdx
    go tree 0 0

data HuffTree
    = Node Int HuffTree HuffTree -- internal node, no associated character
    | Leaf Char Int -- character node
    deriving (Show, Read, Eq)

charToBytes :: Char -> [Word8]
charToBytes c =
    let n = ord c
     in [ fromIntegral
            (n `shiftR` 24)
        , fromIntegral
            (n `shiftR` 16)
        , fromIntegral
            (n `shiftR` 8)
        , fromIntegral
            n
        ]

bytesToChar :: [Word8] -> Char
bytesToChar [b0, b1, b2, b3] =
    chr (fromIntegral b0 `shiftL` 24 .|. fromIntegral b1 `shiftL` 16 .|. fromIntegral b2 `shiftL` 8 .|. fromIntegral b3)
bytesToChar _ = error "bytesToChar: expected 4 bytes"

-- | Lossy preorder serialization of the HuffTree.
serializeTree :: HuffTree -> ByteString
serializeTree root = BS.pack (go root)
  where
    go (Node _ left right) = [0x00] ++ (go left) ++ go (right)
    go (Leaf c _) = [0x01] ++ (charToBytes c)

deserializeTree :: ByteString -> Maybe (HuffTree, ByteString)
deserializeTree bytes = do
    (tree, rest) <- go (BS.unpack bytes)
    Just (tree, BS.pack rest)
  where
    go (0x00 : rest) = do
        -- node
        (left, rest') <- go rest
        (right, rest'') <- go rest'
        Just (Node (-1) left right, rest'')
    go (0x01 : b0 : b1 : b2 : b3 : rest) = Just (Leaf (bytesToChar [b0, b1, b2, b3]) (-1), rest) -- leaf
    go _ = Nothing -- some invalid byte

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

{- | Packs the input Boolean array (the codes) into a ByteString with the following scheme:

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

encode :: Text -> Map Char [Bool] -> Maybe ByteString
encode input tbl =
    let res = map (\c -> Map.lookup c tbl) (T.unpack input)
     in fmap packBits (fmap concat (sequence res))
