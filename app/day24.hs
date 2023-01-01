{-# LANGUAGE TupleSections #-}

module Main where

import           AoC                (applyInput)
import           Data.Bifunctor     (first, second)
import           Data.Function      ((&))
import qualified Data.HashMap.Lazy  as Map
import           Data.HashMap.Lazy  (HashMap)
import           Data.List          (foldl', singleton)
import qualified Data.PQueue.Min    as PQ
import           Data.PQueue.Min    (MinQueue)
import           Text.Parsec.String (Parser)
import           Text.Parsec        (between
                                    ,char
                                    ,choice
                                    ,many
                                    ,many1
                                    ,newline
                                    ,sepEndBy
                                    ,string
                                    ,try)


type Coord  = (Int, Int)
data Dir    = N | S | E | W deriving (Show, Eq)

data Valley = Valley {rows  :: Int
                     ,cols  :: Int
                     ,winds :: HashMap Coord [Dir]
                     } deriving (Show, Eq)

data Path   = Path {steps     :: Int
                   ,toGoal    :: Int
                   ,tile      :: Coord} deriving (Show)


instance Eq Path where
    p1 == p2 = tile p1 == tile p2 && steps p1 == steps p2


instance Ord Path where
    compare p1 p2 =
        case compare (steps p1 + toGoal p1) (steps p2 + toGoal p2) of
            EQ -> compare (toGoal p1) (toGoal p2)
            x  -> x


charToDir :: Char -> Dir
charToDir c =
    case c of
        '^' -> N
        'v' -> S
        '>' -> E
        '<' -> W
        _   -> undefined


dirToFun :: Dir -> Coord -> Coord
dirToFun dir =
    case dir of
        N -> first  (subtract 1)
        S -> first  (+1)
        W -> second (subtract 1)
        E -> second (+1)


distance :: Coord -> Coord -> Int
distance (row1, col1) (row2, col2) =
    abs (row1 - row2) + abs (col1 - col2)


move :: Valley -> Coord -> [Dir] -> [(Coord, Dir)]
move valley pos dirs =
    move1 <$> dirs
  where
    move1     dir        = (wrapCoord (dirToFun dir pos), dir)
    wrapCoord (row, col) = (row `mod` rows valley, col `mod` cols valley)


moveWind :: Valley -> Valley
moveWind valley = valley {winds = Map.foldlWithKey' addWind Map.empty (winds valley)}
  where
    addWind val pos dirs =
        foldl' (\m (c,dir) -> Map.insertWith (++) c [dir] m) val (move valley pos dirs)


possibleMoves :: Coord -> Valley -> [Coord]
possibleMoves coord valley =
    let allPos = coord : (dirToFun <$> [N, S, W, E] <*> [coord])
    in filter valid allPos
  where
    valid pos@(row, col) = not (Map.member pos (winds valley))
                        && (pos == (-1, 0) || pos == (rows valley, cols valley - 1)
                        || ((row >= 0 && row < rows valley)
                            && col >= 0 && col < cols valley))


expand :: Valley -> Coord -> Path -> [Path]
expand valley to p =
    let newTiles  = possibleMoves (tile p) valley
    in map buildPath newTiles
  where
    buildPath c =
        Path {steps    = steps p + 1
            ,toGoal    = distance c to
            ,tile      = c}


search :: [Valley] -> MinQueue Path -> Coord -> Path
search valleys q to =
    case PQ.getMin q of
        Nothing   -> undefined
        Just path
            | tile path == to -> path
            | otherwise       ->
                let valley   = valleys !! (steps path + 1)
                    newPaths = filter (notMember q) $ expand valley to path
                    newQ     = q
                             & PQ.deleteMin
                             & flip (foldl' (flip PQ.insert)) newPaths
                in search valleys newQ to
  where
    notMember pq path = PQ.null $ PQ.filter (== path) pq


astar :: Coord -> Coord -> [Valley] -> Path
astar from to valleys = search valleys (PQ.singleton path0) to
  where
    path0   = Path {steps     = 0
                   ,toGoal    = distance from to
                   ,tile      = from}


solveP2 :: [Valley] -> Int
solveP2 valleys =
    let nRows    = rows $ head valleys
        nCols    = cols $ head valleys
        firstGo  = astar (-1, 0) (nRows, nCols - 1) valleys
        goBack   = astar (nRows, nCols -1) (-1, 0) (drop (steps firstGo) valleys)
        secondGo = astar (-1, 0) (nRows, nCols - 1) (drop (steps firstGo + steps goBack) valleys)
    in steps secondGo + steps goBack + steps firstGo


solveP1 :: [Valley] -> Int
solveP1 valleys = steps $ astar (-1,0) (nRows, nCols - 1) valleys
  where
    nRows = rows (head valleys)
    nCols = cols (head valleys)


rowP :: Parser [(Int, [Dir])]
rowP = do
    row <- between (char '#')
                   (char '#')
                   $ many1 (choice [char '<', char '>', char '^', char 'v', char '.'])
    return $ row
           & zip [0..]
           & filter ((/= '.') . snd)
           & map (second (singleton . charToDir))


valleyP :: Parser [Valley]
valleyP = do
    _      <- string "#."
    nCols  <- length <$> many (char '#')
    _      <- newline
    valley <- try rowP `sepEndBy` newline

    let nRows   = length valley
        vWinds  = concat $ zipWith (\i -> map (first (i, ))) [0..] valley
        valley0 = Valley {rows = nRows, cols = nCols, winds = Map.fromList vWinds}

    return $ iterate moveWind valley0


main :: IO ()
main = applyInput valleyP solveP1 solveP2