module Main where


import AoC                (applyInputSWith)
import Data.List          (foldl', unfoldr)
import Text.Parsec.String (Parser)
import Text.Parsec        (char, choice, many1, newline, sepEndBy)


type Snafu = [Int]


toSnafuChr :: Int -> Char
toSnafuChr n =
    case n of
        0  -> '0'
        1  -> '1'
        2  -> '2'
        -1 -> '-'
        -2 -> '='
        _  -> undefined


intToSnafu :: Int -> Snafu
intToSnafu n
    | n == 0    = [0]
    | otherwise = reverse $ unfoldr extractDigit n
  where
    extractDigit num
        | num == 0  = Nothing
        | otherwise = Just ((num + 2) `mod` 5 - 2, (num + 2) `div` 5)


snafuToInt :: Snafu -> Int
snafuToInt = foldl' ((+) . (*5)) 0


solveP1 :: [Snafu] -> String
solveP1 = map toSnafuChr . intToSnafu . sum . map snafuToInt


snafusP :: Parser [Snafu]
snafusP = snafuP `sepEndBy` newline
  where
    snafuP      = many1 snafuDigitP
    snafuDigitP = choice [char '0' >> return 0
                         ,char '1' >> return 1
                         ,char '2' >> return 2
                         ,char '-' >> return (-1)
                         ,char '=' >> return (-2)
                         ]


main :: IO ()
main = applyInputSWith snafusP () solveP1 (const "") putStrLn putStr