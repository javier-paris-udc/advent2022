{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NumericUnderscores #-}

module Main where


import AoC                (applyInput, commaSepP, intP)
import Data.Function      ((&))
import Data.Maybe         (mapMaybe)
import Data.List          (foldl',  nub, sortOn, find)
import Text.Parsec        (sepEndBy, spaces, string)
import Text.Parsec.String (Parser)


data Coord    = Coord    {x :: Int, y :: Int}            deriving (Show, Eq)
data Interval = Interval {start :: Int, end :: Int}      deriving (Show, Eq)
data Sensor   = Sensor   {pos :: Coord, beacon :: Coord} deriving (Show, Eq)


problemSize :: Int
problemSize = 4_000_000


-- | @mdist c1 c2@ manhattan distance between 'c1' and 'c2'
mdist :: Coord -> Coord -> Int
mdist c1 c2 = abs (c1.x - c2.x) + abs (c1.y - c2.y)


-- Interval Logic

-- | 'intervalLen' length of an interval
intervalLen :: Interval -> Int
intervalLen i = 1 + abs (i.end - i.start)


-- | 'fuse' two intervals
fuse :: Interval -> Interval -> Interval
fuse i1 i2 = Interval {start = min i1.start i2.start, end = max i1.end i2.end}


fuseList :: [Interval] -> Interval -> [Interval]
fuseList [] i       = [i]
fuseList (i1:is) i2
    | overlap i1 i2 = fuse i1 i2:is
    | otherwise      = i2:i1:is


-- | @overlap i1 i2@ is true if 'i1' and 'i2' overlap
overlap :: Interval -> Interval -> Bool
overlap i1 i2 = not (i1.end < i2.start-1 || i2.end+1 < i1.start)


inside :: Int -> Interval -> Bool
inside row i = row >= i.start && row <= i.end


-- Row functions

sensorToRow :: Int -> Sensor -> Maybe Interval
sensorToRow row s
    | rowDistance > radius = Nothing
    | otherwise            = Just Interval {start = s.pos.x - len, end = s.pos.x + len}
  where
    radius      = mdist s.pos s.beacon
    rowDistance = abs (s.pos.y - row)
    len         = radius - rowDistance


beaconsAt :: [Coord] -> [Interval] -> Int
beaconsAt beacons intervals =
    length $ filter (\b -> any (insideC b) intervals) beacons
  where
    insideC :: Coord -> Interval -> Bool
    insideC coord int = coord.x >= int.start && coord.y <= int.end


-- Solver functions

fullRow :: [Interval] -> Bool
fullRow []  = undefined
fullRow [i] = 0 `inside` i && problemSize `inside` i
fullRow _   = False


solveP2 :: [Sensor] -> Int
solveP2 sensors =
    let allSensorIntervals = (mapMaybe . sensorToRow <$> [0..problemSize]) <*> [sensors]
        fusedIntervals     = foldl' fuseList [] . sortOn start <$> allSensorIntervals
        notFull            = find (not . fullRow . fst) (zip fusedIntervals [0..])
    in case notFull of
        Nothing               -> undefined
        Just (intervals, row) -> row + 4_000_000 * findHole intervals
  where
    findHole :: [Interval] -> Int
    findHole [i1, i2]
        | i2.start > i1.end = i1.end + 1
        | otherwise         = i2.end + 1
    findHole _        = undefined



solveP1 :: [Sensor] -> Int
solveP1 sensors =
    let sensorIntervals = sensors
                        & mapMaybe (sensorToRow 2_000_000)
                        & sortOn start
                        & foldl' fuseList []
        intervalLens    = sum $ intervalLen <$> sensorIntervals
        beaconsOnRow    = filter ((== 2_000_000). y) $ nub $ beacon <$> sensors
        beacons         = beaconsAt beaconsOnRow sensorIntervals
    in intervalLens - beacons


-- Parser

sensorsP :: Parser [Sensor]
sensorsP = do
    sensorP `sepEndBy` spaces
  where
    sensorP = do
        _       <- string "Sensor at x="
        sensorX <- intP
        commaSepP
        _       <- string "y="
        sensorY <- intP
        _       <- string ":" >> spaces >> string "closest beacon is at x="
        beaconX <- intP
        commaSepP
        _       <- string "y="
        beaconY <- intP
        return $ Sensor {pos = Coord sensorX sensorY, beacon = Coord beaconX beaconY}


main :: IO ()
main = applyInput sensorsP solveP1 solveP2