module Lib (compress) where

import Data.List (insert, sort)
import Data.Map (Map)
import qualified Data.Map as Map

compress :: String -> Maybe String
compress input = do
    tree <- buildTree input
    let ct = buildCodeTable tree
    encode input ct

data HuffTree
    = Node Int HuffTree HuffTree
    | Leaf Char Int
    deriving (Show, Read, Eq)

freq :: String -> Map Char Int
freq str = Map.fromListWith (+) (map (\c -> (c, 1)) str)

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

buildTree :: String -> Maybe HuffTree
buildTree input =
    let freqMap = freq input
        nodes = toLeaves freqMap
        sorted = sort nodes
     in merge sorted

buildCodeTable :: HuffTree -> Map Char String
buildCodeTable tree = go [] tree
  where
    go path (Leaf c _) = Map.singleton c path
    go path (Node _ left right) =
        let leftTree = go (path ++ "0") left
            rightTree = go (path ++ "1") right
         in Map.union leftTree rightTree

encode :: String -> Map Char String -> Maybe String
encode input tbl =
    let res = map (\c -> Map.lookup c tbl) input
     in fmap concat (sequence res)

-- decode :: String -> Map Char String -> Maybe String
-- decode input tab = Just "todo"
