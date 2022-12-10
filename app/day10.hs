module Main where

import AoC            (applyInputSWith, blanksP, intP)
import Text.Parsec    (Parsec
                      ,(<|>)
                      ,getState
                      ,modifyState
                      ,newline
                      ,sepEndBy
                      ,string)
import Data.Bifunctor (bimap, first)
import Data.List      (intercalate, intersperse, unfoldr)
import Data.Vector    (Vector, (!), fromList)


type Cycle     = Int
type CPUState  = (Cycle, Int)
type RegValues = Vector Int


groupIn :: Int -> [a] -> [[a]]
groupIn n = unfoldr (\l -> if null l then Nothing else Just $ splitAt n l)


solveP2 :: RegValues -> String
solveP2 v = intercalate "\n" $ groupIn 40 $ map pixelAt [0..239]
  where
    pixelAt i
        | distance (v ! i) (i `rem` 40) < 2  = '#'
        | otherwise                          = ' '
    distance x y = abs (x - y)


solveP1 :: RegValues -> Int
solveP1 v = sum $ map (\i -> i * v `at` i) [20, 60, 100, 140, 180, 220]
  where
    at v i = v ! (i - 1)


noopP :: Parsec String CPUState [Int]
noopP = do
    string "noop"
    v <- snd <$> getState
    modifyState (first (+1))
    return [v]


addxP :: Parsec String CPUState [Int]
addxP = do
    string "addx"
    blanksP
    inc <- intP
    v1  <- snd <$> getState
    modifyState (bimap (+2) (+inc))
    v2  <- snd <$> getState
    return [v1, v2]


instP :: Parsec String CPUState [Int]
instP = noopP <|> addxP


regChangesP :: Parsec String CPUState RegValues
regChangesP = fromList . (1:) . concat <$> instP `sepEndBy` newline


main :: IO ()
main = applyInputSWith regChangesP (1, 1) solveP1 solveP2 print putStrLn