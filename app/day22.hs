module Main where


import           AoC                (applyInput, intP)
import           Data.Array         (Array, (!), assocs, listArray)
import           Data.Bifunctor     (bimap, first, second)
import           Data.List          (find, foldl', transpose, unfoldr)
import           Data.Map           (Map)
import qualified Data.Map           as Map
import           Data.Maybe         (fromJust)
import           Text.Parsec        (char, choice, many, newline, sepEndBy)
import           Text.Parsec.String (Parser)


type Coord     = (Int, Int)
data Tile      = Wall | Dot | Empty deriving (Show, Eq)
data Dir       = E | S | W | N deriving (Show, Eq, Enum, Ord)
data Move      = Go Int | L | R deriving (Show, Eq)
type BorderMap = Map (Coord, Dir) (Coord, Dir, Bool)


dirToFun :: Dir -> Coord -> Coord
dirToFun dir =
    case dir of
        N -> first (subtract 1)
        S -> first (+1)
        W -> second  (subtract 1)
        E -> second  (+1)


advance :: (Coord -> Dir -> (Coord, Dir))
        -> Int
        -> (Coord, Dir)
        -> Array Coord Tile
        -> (Coord, Dir)
advance next n (pos, dir) b
    | n == 0    = (pos, dir)
    | otherwise = let newPos = next pos dir
                  in (if b ! fst newPos == Wall then (pos, dir)
                      else advance next (n-1) newPos b)


rotate :: Move -> Dir -> Dir
rotate R      N   = E
rotate R      dir = succ dir
rotate L      E   = N
rotate L      dir = pred dir
rotate (Go _) dir = dir


square :: Int -> Coord -> Coord
square side = bimap ((+1). (`div` side) . subtract 1) ((+1). (`div` side) . subtract 1)


bordersP2 :: BorderMap
bordersP2 = Map.fromList [(((1, 2), N), ((4, 1), W, False))
                         ,(((1, 2), W), ((3, 1), W, True)) -- inverted
                         ,(((1, 3), N), ((4, 1), S, False))
                         ,(((1, 3), S), ((2, 2), E, False))
                         ,(((1, 3), E), ((3, 2), E, True)) -- inverted
                         ,(((2, 2), W), ((3, 1), N, False))
                         ,(((2, 2), E), ((1, 3), S, False))
                         ,(((3, 2), E), ((1, 3), E, True)) -- inverted
                         ,(((3, 2), S), ((4, 1), E, False))
                         ,(((3, 1), N), ((2, 2), W, False))
                         ,(((3, 1), W), ((1, 2), W, True)) -- inverted
                         ,(((4, 1), W), ((1, 2), N, False))
                         ,(((4, 1), E), ((3, 2), S, False))
                         ,(((4, 1), S), ((1, 3), N, False))
                         ]

bordersP1 :: BorderMap
bordersP1 = Map.fromList[(((1, 2), N), ((3, 2), S, False))
                        ,(((1, 2), W), ((1, 3), E, False))
                        ,(((1, 3), N), ((1, 3), S, False))
                        ,(((1, 3), S), ((1, 3), N, False))
                        ,(((1, 3), E), ((1, 2), W, False))
                        ,(((2, 2), W), ((2, 2), E, False))
                        ,(((2, 2), E), ((2, 2), W, False))
                        ,(((3, 2), E), ((3, 1), W, False))
                        ,(((3, 2), S), ((1, 2), N, False))
                        ,(((3, 1), N), ((4, 1), S, False))
                        ,(((3, 1), W), ((3, 2), E, False))
                        ,(((4, 1), W), ((4, 1), E, False))
                        ,(((4, 1), E), ((4, 1), W, False))
                        ,(((4, 1), S), ((3, 1), N, False))
                        ]



checkInvert :: Int -> Bool -> Int -> Int
checkInvert side inverted val
    | inverted  = abs (side - val) + 1
    | otherwise = val


crossBorder :: Int -> (Coord, Dir, Bool) -> Int -> (Coord, Dir)
crossBorder side ((x, y), enterDir, invert) val =
    case enterDir of
        N -> (((x - 1) * side + 1, (y - 1) * side + checkInvert side invert val) , oppD)
        S -> ((x * side, (y - 1) * side + checkInvert side invert val), oppD)
        E -> (((x - 1) * side + checkInvert side invert val, y * side), oppD)
        W -> (((x - 1) * side + checkInvert side invert val, (y -1) * side + 1), oppD)
  where
    oppD = case enterDir of
        N -> S
        S -> N
        E -> W
        W -> E


buildNext :: Int
          -> [Coord]
          -> BorderMap
          -> Coord
          -> Dir
          -> (Coord, Dir)
buildNext side faces borders c@(row, col) dir
    | cSq == nextSq       = (move c, dir)
    | nextSq `elem` faces = (move c, dir)
    | otherwise           = crossBorder side (borders Map.! (cSq, dir)) borderCoord
  where
    move   = dirToFun dir
    cSq    = square side c
    nextSq = square side $ move c
    borderCoord
        | dir == N || dir == S = (col - 1) `mod` side + 1
        | otherwise            = (row - 1) `mod` side + 1


doMove :: (Coord -> Dir -> (Coord, Dir))
       -> Array Coord Tile
       -> (Coord, Dir)
       -> Move
       -> (Coord, Dir)
doMove next b pos mov =
    case mov of
        Go     n -> advance next n pos b
        rotation -> second (rotate rotation) pos


password :: (Coord, Dir) -> Int
password ((x, y), dir) = x * 1000 + y * 4 + fromEnum dir


solve :: Array Coord Tile -> [Move] -> (Coord -> Dir -> (Coord, Dir)) -> Int
solve b moves next = password $ foldl' (doMove next b) (start, E) moves
  where
    start = fst . fromJust $ find ((/=Empty) . snd) $ assocs b


solveP2 :: (Array Coord Tile, [Move], BorderMap -> Coord -> Dir -> (Coord, Dir)) -> Int
solveP2 (b, moves, next) = solve b moves (next bordersP2)


solveP1 :: (Array Coord Tile, [Move], BorderMap -> Coord -> Dir -> (Coord, Dir)) -> Int
solveP1 (b, moves, next) = solve b moves (next bordersP1)



-- Parser

isqrt :: Int -> Int
isqrt n = aux 1 n
  where
    aux low high
        | mid * mid == n  = mid
        | mid * mid < n   = aux (mid + 1) high
        | otherwise       = aux low (mid - 1)
      where
        mid = (low + high) `div` 2


groupsOf :: Int -> [a] -> [[a]]
groupsOf n = unfoldr (\l -> if null l then Nothing else Just $ splitAt n l)


cubeP :: Parser (Array Coord Tile, [Move], BorderMap -> Coord -> Dir -> (Coord, Dir))
cubeP = do
    cubeRows <- rowP `sepEndBy` newline
    movs <- many movP

    let rowLens    = map length cubeRows
        nCols      = maximum rowLens
        paddedRows = zipWith (grow nCols) cubeRows rowLens

        size    = sum $ map (length . filter (/=Empty)) cubeRows
        sqSide  = isqrt (size `div` 6)
        divided = map (map concat . transpose) $ groupsOf sqSide
                $ map (groupsOf sqSide) cubeRows
        numbered = zipWith (\i row -> zipWith (\j sq -> ((i,j), sq)) [1..] row)
                           [1..]
                           divided
        faces   = concatMap (filter ((/=Empty) .head.snd)) numbered
        maps    = buildNext sqSide $ map fst faces

    return (listArray ((1,1) , (length cubeRows, nCols)) (concat paddedRows), movs, maps)

  where
    rowP  = many tileP
    tileP = choice [char ' ' >> return Empty
                   ,char '.' >> return Dot
                   ,char '#' >> return Wall
                   ]
    movP  = choice [Go <$> intP
                   ,char 'L' >> return L
                   ,char 'R' >> return R
                   ]
    grow nCols row len
        | len < nCols = row ++ replicate (nCols - len) Empty
        | otherwise  = row


main :: IO ()
main = applyInput cubeP solveP1 solveP2