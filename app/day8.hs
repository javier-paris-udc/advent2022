{-# LANGUAGE TupleSections #-}
module Main where

import AoC                 (applyInput)
import Control.Applicative (liftA2)
import Data.Array          ((!), Array, bounds, indices, listArray)
import Data.Function       (on)
import Data.List           (singleton)
import Text.Parsec.String  (Parser)
import Text.Parsec         (digit, many1, sepEndBy1, spaces)


type Coord = (Int, Int)
type TreeGrid = Array Coord Int


treesToBorder :: TreeGrid -> Coord -> [[Coord]]
treesToBorder trees (x, y) =
    [map (, y) [x + 1        .. maxX]
    ,map (x, ) [y + 1        .. maxY]
    ,map (, y) [x - 1, x - 2 ..    0]
    ,map (x, ) [y - 1, y - 2 ..    0]
    ]
  where
    (maxX, maxY) = snd $ bounds trees


takeUpTo :: (a -> Bool) -> [a] -> [a]
takeUpTo p = foldr upTo []
  where
    upTo x xs
      | p x       = x:xs
      | otherwise = [x]


taller :: TreeGrid -> Coord -> Coord -> Bool
taller trees = (>) `on` (trees !)


visible :: TreeGrid -> Coord -> Bool
visible trees t = any (all $ taller trees t) (treesToBorder trees t)


scenicScore :: TreeGrid -> Coord -> Int
scenicScore trees t =
    product $ map (length . takeUpTo (taller trees t)) (treesToBorder trees t)


solveP2 :: TreeGrid -> Int
solveP2 = maximum . liftA2 map scenicScore indices


solveP1 :: TreeGrid -> Int
solveP1 = length . liftA2 filter visible indices


arrayP :: Parser TreeGrid
arrayP = do
    rows <- rowP `sepEndBy1` spaces
    let rowsLen = length rows
        colsLen = length $ head rows
    return $ listArray ((0, 0), (rowsLen - 1, colsLen -1)) (concat rows)
  where
    rowP = many1 (read . singleton <$> digit)


main :: IO ()
main = applyInput arrayP () solveP1 solveP2