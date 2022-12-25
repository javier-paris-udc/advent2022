{-# LANGUAGE BangPatterns #-}
module Main where

import           AoC                 (applyInput, blanksP, commaSepP, intP)
import           Control.Applicative (liftA2)
import           Data.Bifunctor      (bimap, second)
import           Data.HashSet        (HashSet, fromList)
import qualified Data.HashSet        as Set
import           Data.List           (find)
import           Text.Parsec         (newline, sepBy, sepEndBy, string)
import           Text.Parsec.String  (Parser)

type Coord   = (Int, Int)
type CaveMap = HashSet Coord


source :: Coord
source = (500, 0)


s :: Coord -> Coord
s = second (+1)

sw :: Coord -> Coord
sw = bimap (subtract 1) (+1)

se :: Coord -> Coord
se = bimap (+1) (+1)


repeatDropSand :: (Coord -> CaveMap -> Bool) -> Int -> CaveMap -> Int
repeatDropSand atEnd floorLevel =
    iter 0
  where
    iter !i cave =
        case dropSand cave floorLevel source of
           x | atEnd x cave -> i
             | otherwise    -> iter (i+1) (Set.insert x cave)


dropSand :: CaveMap -> Int -> Coord -> Coord
dropSand cave maxHeight c
    | maxHeight == snd c = c
    | otherwise          =
        case find (not . (`Set.member` cave)) $  [s, sw, se] <*> [c] of
            Nothing  -> c
            Just dir -> dropSand cave maxHeight dir


solveP2 :: CaveMap -> Int
solveP2 cave = repeatDropSand (\_ caveSt -> Set.member source caveSt) floorLevel cave
  where
    floorLevel = 1 + maximum (Set.map snd cave)


solveP1 :: CaveMap -> Int
solveP1 cave = repeatDropSand (\c _ -> snd c == abyss) abyss cave
  where
    abyss = maximum $ Set.map snd cave


-- Parser

structureP :: Parser [Coord]
structureP = do
    coords <- coordP `sepBy` (blanksP >> string "->" >> blanksP)
    return $ concat $ zipWith endsToCoords coords (drop 1 coords)
  where
    coordP = do
        x <- intP
        commaSepP
        y <- intP
        return (x, y)

    endsToCoords (x1, y1) (x2, y2) = liftA2 (,) (numsBetween x1 x2) (numsBetween y1 y2)

    numsBetween a b
        | a <= b    = [a .. b]
        | otherwise = [b .. a]


caveP :: Parser CaveMap
caveP = do
    structs <- structureP `sepEndBy` newline
    return $ fromList (concat structs)



main :: IO ()
main = applyInput caveP solveP1 solveP2