{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TupleSections #-}

module Main where


import           AoC                      (applyInput)
import           Control.Monad            (unless)
import           Data.Bifunctor           (bimap, first, second)
import           Data.HashMap.Strict      (HashMap, (!))
import qualified Data.HashMap.Strict      as Map
import           Data.HashSet             (HashSet)
import qualified Data.HashSet             as Set
import           Data.Maybe               (fromMaybe)
import           Data.List                (intercalate)
import           Text.Parsec              (many, (<|>), char)
import           Text.Parsec.String       (Parser)
import           Control.Monad.State.Lazy (State, execState, get, gets, modify, put)


type Coord = (Int, Int)
type Move  = Coord -> Coord
type Board = HashSet Coord
type Rock = HashSet Coord
type HeightMap = HashMap Int Int


left, right, down :: Move
left  = first  (subtract 1)
right = first  (+1)
down  = second (subtract 1)


lineRock, crossRock, lRock, iRock, sqRock :: Rock
lineRock  = Set.fromList [(0,0), (1,0), (2,0), (3,0)]
crossRock = Set.fromList [(1,0), (0,1), (1,1), (2,1), (1,2)]
lRock     = Set.fromList [(0,0), (1,0), (2,0), (2,1), (2,2)]
iRock     = Set.fromList [(0,0), (0,1), (0,2), (0,3)]
sqRock    = Set.fromList [(0,0), (0,1), (1,0), (1,1)]


pieces :: [Rock]
pieces = cycle [lineRock, crossRock, lRock, iRock, sqRock]


rockHeight :: Rock -> Int
rockHeight b
     | Set.null b = 0
     | otherwise  = 1 + maximum (Set.map snd b)


moveRock :: Board -> Rock -> Move -> Maybe Rock
moveRock board rock mv
    |  all clear newRock && all inBoard newRock = Just newRock
    |  otherwise                                = Nothing
  where
    newRock        = Set.map mv rock
    clear          = not . (`Set.member` board)
    inBoard (x, y) = x >= 0 && x <= 6 && y >= 0


base :: Rock -> Int
base = minimum . Set.map snd


showBoard :: Int -> Int -> Board -> String
showBoard y1 y2 brd = intercalate "\n" $ map showLine [y2, y2-1..y1]
  where
    showLine y = map (\x -> if Set.member (x,y) brd then '#' else '.') [0..6]


updateHeight :: Rock -> Int -> Int -> Int
updateHeight rock i ht =
    let columnI = Set.filter ((==i) . fst) rock
    in if Set.null columnI
       then ht
       else max ht (maximum (Set.map snd columnI))


updateHeights :: HeightMap -> Rock -> HeightMap
updateHeights hm rock =
    let hmWithRock = Map.mapWithKey (updateHeight normalizedRock) hm
    in normalizeHeightMap hmWithRock
  where
    normalizeRockY        = subtract (base rock) . (+heightMapAtRock) . (+ 1)
    normalizedRock        = Set.map (second normalizeRockY) rock
    normalizeHeightMap hm = Map.map (subtract (minimum hm)) hm
    heightMapAtRock       =
        let rockXs = Set.map fst rock
        in maximum $ Map.filterWithKey (\k _ -> k `Set.member` rockXs) hm


data RockState = RockState {moveL   :: [Move]
                           ,board   :: Board
                           ,height  :: Int
                           ,moves   :: Int
                           ,nRocks  :: Int
                           ,posits  :: HashMap (Int, Int, HeightMap) (Int, Int, Int)
                           ,heights :: HeightMap
                           ,repeats :: Bool
                           }


dropRock :: Int -> Rock -> State RockState ()
dropRock movesLen rock = do
    move <- gets (head . moveL)
    brd  <- gets board
    st   <- get

    let horizontalRock = fromMaybe rock $ moveRock brd rock move
        newRock        = fromMaybe horizontalRock $ moveRock brd horizontalRock down

        newMoves       = st.moves + 1
        newNRocks      = st.nRocks + 1
        newHeight      = max st.height (rockHeight newRock)
        newHeightMap   = updateHeights st.heights newRock
        positsKey      = (newMoves `rem` movesLen, newNRocks `rem` 5, newHeightMap)
        positsVal      = (newMoves, newNRocks, newHeight)

        newSt          = st {moveL   = tail st.moveL
                            ,board   = newRock `union` brd
                            ,height  = newHeight
                            ,moves   = newMoves
                            ,nRocks  = newNRocks
                            ,heights = updateHeights st.heights newRock
                            }

    if  | base newRock == base rock && newNRocks `rem` 5 == 0 ->
            put $ newSt {posits  = Map.insertWith (const id) positsKey positsVal st.posits
                        ,repeats = Map.member positsKey st.posits}
        | base newRock == base rock  ->
            put newSt
        | otherwise                  ->
            put (st {moveL = tail st.moveL, moves = st.moves +1})
            >> dropRock movesLen newRock
  where
    union s1 s2 = foldr Set.insert s2 s1


dropRockOnBoard :: Int -> Rock -> State RockState ()
dropRockOnBoard movesLen rock = do
    dropHeight <- (+3) <$> gets height
    dropRock movesLen $ Set.map (bimap (+2) (+dropHeight)) rock


dropRocks :: State RockState Bool -> [Rock] -> Int -> [Move] -> RockState
dropRocks check rocks movesLen moves =
    execState (foldUntilM_ (dropRockOnBoard movesLen <$> rocks) check) initState
  where
    heightMap = Map.fromList ((, 0) <$> [0..6])
    initState = RockState {moveL   = moves
                          ,board   = Set.empty
                          ,height  = 0
                          ,moves   = 0
                          ,nRocks  = 0
                          ,posits  = Map.empty
                          ,heights = heightMap
                          ,repeats = False}


foldUntilM_ :: (Monad m) => [m a] -> m Bool -> m ()
foldUntilM_ []     _ = return ()
foldUntilM_ (x:xs) p = do
    x
    cond <- p
    unless cond $ foldUntilM_ xs p


solveP2 :: [Move] -> Int
solveP2 m = afterHeight + prevHeight + blockHeight * blocks
  where
    movesLen      = length m
    st            = dropRocks (gets repeats) pieces movesLen (cycle m)
    positsKey     = (st.moves `rem` movesLen, st.nRocks `rem` 5, st.heights)
    ( prevMovs
     ,prevRocks
     ,prevHeight) = st.posits !  positsKey

    rocksLeft     = 1_000_000_000_000 - prevRocks
    blockRocks    = st.nRocks - prevRocks
    blockHeight   = st.height - prevHeight

    blocks        = rocksLeft `div` blockRocks
    rocksAfter    = rocksLeft `rem` blockRocks

    afterHeight   = solve (rocksAfter + st.nRocks) (cycle m) - st.height


solve :: Int -> [Move] -> Int
solve n moves = height $ dropRocks (return False) (take n pieces) (length moves) moves


solveP1 :: [Move] -> Int
solveP1 = solve 2022 . cycle


windP :: Parser [Move]
windP = many (leftP <|> rightP)
  where
    leftP  = char '<' >> return left
    rightP = char '>' >> return right


main :: IO ()
main = applyInput windP solveP1 solveP2