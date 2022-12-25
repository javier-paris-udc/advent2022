{-# LANGUAGE TupleSections #-}
module Main where

import           AoC            (applyInputS)
import           Data.Array     (Array
                                ,(!)
                                ,bounds
                                ,Ix (inRange)
                                ,listArray
                                )
import           Data.Bifunctor (first, second)
import           Data.Char      (ord)
import           Data.Maybe     (fromJust)
import           Djistra        (djistra)
import           Text.Parsec    (Parsec
                                ,getState
                                ,lower
                                ,(<|>)
                                ,char
                                ,many1
                                ,sepEndBy
                                ,newline
                                ,modifyState)


type Coord = (Int, Int)
type Elevation = Array Coord Int


up :: Coord -> Coord
up = first (subtract 1)


down :: Coord -> Coord
down = first (+1)


left :: Coord -> Coord
left = second (subtract 1)


right :: Coord -> Coord
right = second (+1)


moves :: (Int -> Int -> Bool) ->  Elevation -> Coord -> [(Coord, Int)]
moves elevationTest elevation c =
    map (,1) $ filter canMove $ [up, down, left, right] <*> [c]
  where
    canMove x = inRange (bounds elevation) x
             && elevationTest (elevation ! c) (elevation ! x)



-- Solver functions

solveP2 :: ((Coord, Coord), Elevation) -> Int
solveP2 ((_, end), elevation) =
    subtract 1 $ length $ fromJust $  djistra end isA (moves downOne elevation)
  where
    isA         = (==0).(elevation !)
    downOne x y = x <= y + 1


solveP1 :: ((Coord, Coord), Elevation) -> Int
solveP1 ((start, end), elevation) =
    subtract 1 $ length $  fromJust $ djistra start (==end) (moves upOne elevation)
  where
    upOne x y = x >= y - 1


-- Parsing

type ParserState = (Int, (Maybe Int, Maybe Int))  -- pos, (start, end)
type Parser = Parsec String ParserState


heightCharP :: Parser Int
heightCharP = do
    c <- lower <|> startP <|> endP
    modifyState (first (+1))
    return (ord c - ord 'a')
  where
    startP = do
        _   <- char 'S'
        pos <- fst <$> getState
        modifyState $ second $ first $ const $ Just pos
        return 'a'
    endP = do
        _   <- char 'E'
        pos <- fst <$> getState
        modifyState $ second $ second $ const $ Just pos
        return 'z'


elevationP :: Parser ((Coord, Coord), Elevation)
elevationP = do
    elevation <- many1 heightCharP `sepEndBy` newline
    let xlen = length elevation
        ylen = length (head elevation)
    (Just startIx, Just endIx) <- snd <$> getState
    let start = (startIx `div` ylen, startIx `rem` ylen)
        end   = (endIx   `div` ylen, endIx   `rem` ylen)
        arr   = listArray ((0, 0), (xlen - 1, ylen - 1)) $ concat elevation
    return ((start, end), arr)


main :: IO ()
main = applyInputS elevationP (0, (Nothing, Nothing)) solveP1 solveP2