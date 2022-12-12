{-# LANGUAGE TupleSections #-}
module Main where

import AoC (applyInputS)
import Text.Parsec (Parsec, getState, putState, lower, (<|>), char, many1, sepEndBy, newline, modifyState)
import Data.Bifunctor (first, second, bimap)
import Data.Char (ord)
import Data.Array (Array, listArray, Ix (inRange), bounds, (!), assocs)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Sequence (ViewL((:<)), Seq)
import Data.Foldable (foldl')
import Data.Maybe (fromJust, catMaybes, mapMaybe)


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


moveFrom :: Elevation -> Coord -> [(Coord, Int)]
moveFrom elevation c =
    map (,1) $ filter canMove $ [up, down, left, right] <*> [c]
  where
    canMove x = inRange (bounds elevation) x
             && elevation ! x <= elevation ! c + 1


binInsert :: (Ord b) => Seq ([a], b) -> ([a], b) -> Seq ([a], b)
binInsert seq x@(path, cost) =
    Seq.insertAt pos x seq
  where
    pos = findPos 0 (Seq.length seq)
    findPos left right
        | left == right                   = left
        | snd (Seq.index seq mid) == cost = mid
        | snd (Seq.index seq mid) <  cost = findPos (mid + 1) right
        | otherwise                       = findPos left (mid - 1)
      where
        mid = (left + right) `div` 2


djistra :: (Eq a, Ord a, Ord b, Num b) => a -> a -> (a -> [(a, b)]) -> Maybe [a]
djistra start end adj =
    runDjistra (Seq.singleton ([start], 0)) (Set.singleton start)
  where
    runDjistra next visited = case Seq.viewl next of
        Seq.EmptyL       -> Nothing
        (path@(pos : _), cost) :< rest
            | pos == end -> Just (reverse path)
            | otherwise  -> let newVertex  = expandPath visited pos
                                newPaths   = bimap (:path) (+cost) <$> newVertex
                                newVisited = Set.insert pos visited
                                nextNoPos  = Seq.filter ((/= pos) . head . fst) next
                                newNext    = foldl' binInsert nextNoPos newPaths
                            in runDjistra newNext newVisited
    expandPath visited pos = filter ((`Set.notMember` visited) . fst) $ adj pos


solve :: ((Coord, Coord), Elevation) -> Maybe Int
solve ((start, end), elevation) =
    subtract 1.length <$> djistra start end (moveFrom elevation)


solveP2 :: ((Coord, Coord), Elevation) -> Int
solveP2 ((_, end), elevation) =
    minimum $ mapMaybe (\c -> solve ((c, end), elevation)) as
  where
    as = map fst $ filter ((==0).snd) $ assocs elevation


solveP1 :: ((Coord, Coord), Elevation) -> Int
solveP1 = fromJust.solve


type ParserState = (Int, (Maybe Int, Maybe Int))  -- pos, (start, end)
type Parser = Parsec String ParserState


heightCharP :: Parser Int
heightCharP = do
    c <- lower <|> startP <|> endP
    modifyState (first (+1))
    return (ord c - ord 'a')
  where
    startP = do
        char 'S'
        pos <- fst <$> getState
        modifyState $ second $ first $ const $ Just pos
        return 'a'
    endP = do
        char 'E'
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