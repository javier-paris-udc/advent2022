module Main where

import AoC                (applyInput, intP)
import Data.Biapplicative (biliftA2)
import Data.Bifunctor     (bimap, first, second)
import Data.List          (nub, scanl')
import Data.Set           (fromList, size)
import Text.Parsec.String (Parser)
import Text.Parsec        (choice, sepEndBy, spaces, string)


type Coord = (Int, Int)
type Move  = Coord -> Coord


up :: Move
up = first (subtract 1)


down :: Move
down = first (+1)


left :: Move
left = second (subtract 1)


right :: Move
right = second (+1)


distance :: Coord -> Coord -> Int
distance (x1, y1) (x2, y2) = max (abs (x1 - x2)) (abs (y1 - y2))


follow :: Coord -> Coord -> Coord
follow c1 c2
    | distance c1 c2 < 2 = c2
    | otherwise          = c2 `moveTowards` c1
  where
    moveTowards    = biliftA2 moveAxis moveAxis
    moveAxis v1 v2 = v1 + signum (v2 - v1)


move :: [Coord] -> Move -> [Coord]
move (h:rest) mov = newRope
  where
    newRope = mov h : zipWith follow newRope rest


solveP2 :: [Move] -> Int
solveP2 = size . fromList . map last . scanl' move (replicate 10 (0, 0))


solveP1 :: [Move] -> Int
solveP1 = size . fromList . map last . scanl' move (replicate 2 (0, 0))


movesP :: Parser [Move]
movesP = concat <$> moveP `sepEndBy` spaces
  where
    moveP = choice [string "U " >> flip replicate up    <$> intP
                   ,string "D " >> flip replicate down  <$> intP
                   ,string "L " >> flip replicate left  <$> intP
                   ,string "R " >> flip replicate right <$> intP
                   ]

main :: IO ()
main = applyInput movesP solveP1 solveP2