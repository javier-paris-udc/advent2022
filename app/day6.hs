module Main where
import Text.Parsec.String (Parser)
import Text.Parsec        (letter, many1)
import AoC                (applyInput)
import Data.List          (findIndex, group, sort, tails)
import Data.Maybe         (fromJust)


nonRepeating :: (Eq a, Ord a) => [a] -> Bool
nonRepeating = all ((==1).length) . group . sort


findNonRepeat :: Int -> String -> Int
findNonRepeat n =
    (+n) . fromJust . findIndex nonRepeating . map (take n) . tails


solveP2 :: String -> Int
solveP2 = findNonRepeat 14


solveP1 :: String -> Int
solveP1 = findNonRepeat 4


stringP :: Parser String
stringP = many1 letter


main :: IO ()
main = applyInput stringP solveP1 solveP2