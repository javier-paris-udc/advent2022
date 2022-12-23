module Main where


import           AoC                 (applyInput)
import           Data.Bifunctor      (first, second)
import           Data.Function       ((&), on)
import           Data.Function.HT    (nest)
import           Data.HashMap.Strict (HashMap, (!?))
import qualified Data.HashMap.Strict as Map
import           Data.HashSet        (HashSet)
import qualified Data.HashSet        as Set
import           Data.List           (find, foldl')
import           Text.Parsec.String  (Parser)
import           Text.Parsec         ((<|>), char, many1, sepEndBy, spaces)


type Coord = (Int, Int)
type Board = HashSet Coord
data Dir   = N | S | W | E deriving (Show, Eq, Enum)


nextDir :: Dir -> Dir
nextDir E = N
nextDir d = succ d


-- | isInDir dir c1 c2 is true if c2 is in direction dir of c1
isInDir :: Dir -> Coord -> Coord -> Bool
isInDir d =
    case d of
        N -> (>) `on` fst
        S -> (<) `on` fst
        E -> (<) `on` snd
        W -> (>) `on` snd


move :: Dir -> Coord -> Coord
move d =
    case d of
        N -> first (subtract 1)
        S -> first (+1)
        E -> second (+1)
        W -> second (subtract 1)


neighbours :: HashSet Coord -> Coord -> [Coord]
neighbours board (x, y) =
    filter (`Set.member` board) neighCoords
  where
    neighCoords = [(x - 1, y - 1), (x - 1, y), (x - 1, y + 1)
                  ,(x,     y - 1),             (x,     y + 1)
                  ,(x + 1, y - 1), (x + 1, y), (x + 1, y + 1)]


nextPos :: HashSet Coord -> [Dir] -> HashMap Coord Coord -> Coord -> HashMap Coord Coord
nextPos currentBoard dirs nextBoard c
    | null cNeighbours = Map.insert c c nextBoard
    | otherwise =
        case freeDir of
            Nothing  -> Map.insert c c nextBoard
            Just dir ->
                let next = move dir c
                in case nextBoard !? next of
                    Nothing    -> Map.insert next c nextBoard
                    Just other -> nextBoard
                                & Map.delete next
                                & Map.insert other other
                                & Map.insert c c
  where
    cNeighbours = neighbours currentBoard c
    freeDir     = find (\d -> not $ any (isInDir d c) cNeighbours) dirs


turn :: (Board, Dir) -> (Board, Dir)
turn (b, d) =
    (Map.keysSet (foldl' (nextPos b dirs) Map.empty b), nextDir d)
  where
    dirs = take 4 $ iterate nextDir d


solveP2 :: Board -> Int
solveP2 board = repeatUntilEqual board (turn (board, N)) 1
  where
    repeatUntilEqual prevB (b, dir) n
        | b == prevB = n
        | otherwise  = repeatUntilEqual b (turn (b, dir)) (n + 1)


solveP1 :: Board -> Int
solveP1 b =
    (maximum xs - minimum xs + 1) * (maximum ys - minimum ys + 1) - Set.size finalBoard
  where
    (finalBoard, _ ) = nest 10 turn (b, N)
    xs               = Set.map fst finalBoard
    ys               = Set.map snd finalBoard


elfMapP :: Parser Board
elfMapP = do
    rows <- rowP `sepEndBy` spaces
    let idxSet = rows
               & map (zip [0..])
               & zipWith (map . first . (,)) [0..]
               & concat
               & filter ((== '#') . snd)
               & map fst

    return $ Set.fromList idxSet
  where
    rowP = many1 (char '.' <|> char '#')


main :: IO ()
main = applyInput elfMapP solveP1 solveP2