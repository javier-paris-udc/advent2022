module Main where


import           AoC                (applyInput, intP)
import qualified Data.IntMap        as Map
import           Data.IntMap        ((!), IntMap)
import           Data.List          (foldl', foldl1')
import           Data.Maybe         (fromJust)
import qualified Data.Sequence      as Seq
import           Data.Sequence      (Seq)
import           Text.Parsec        (sepEndBy, spaces)
import           Text.Parsec.String (Parser)


mixOne :: IntMap Int -> Seq Int -> Int -> Seq Int
mixOne m seqToMix i
    | from == to = seqToMix
    | otherwise  = Seq.insertAt to i $ Seq.deleteAt from seqToMix
  where
    from = fromJust $ Seq.findIndexL (==i) seqToMix
    to   = (from + m ! i) `mod` (Map.size m - 1)

mixAll :: IntMap Int -> Seq Int -> Seq Int
mixAll m seqToMix = foldl' (mixOne m) seqToMix [0 .. Map.size m - 1]


solve :: Int -> [Int] -> Int
solve n l = sum $ map (Seq.index resSeq) positions
  where
    m         = Map.fromAscList $ zip [0 .. ] l
    seqToMix  = Seq.fromList [0 .. Map.size m - 1]

    mix       = foldl1' (.) $ replicate n (mixAll m)
    resSeq    = (m !) <$> mix seqToMix

    pos0      = fromJust $ Seq.findIndexL (==0) resSeq
    positions = (`mod` Map.size m) <$> [pos0 + 1000, pos0 + 2000, pos0 + 3000]


solveP2 :: [Int] -> Int
solveP2 = solve 10 . map (* 811589153)


solveP1 :: [Int] -> Int
solveP1 = solve 1


numbersP :: Parser [Int]
numbersP = intP `sepEndBy` spaces


main :: IO ()
main = applyInput numbersP solveP1 solveP2