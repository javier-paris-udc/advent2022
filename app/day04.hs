module Main where

import AoC                (applyInput, intP)
import Text.Parsec.String (Parser)
import Text.Parsec        (sepEndBy1, newline, char)


type Sec     = (Int, Int)
type SecPair = (Sec, Sec)


within :: Int -> Sec -> Bool
within x (l, r) = x >= l && x <= r


overlap :: SecPair -> Bool
overlap (p1@(from1, to1) , p2@(from2, to2)) =
    from1 `within` p2 || to1 `within` p2 || from2 `within` p1 || to2 `within` p1


contained :: SecPair -> Bool
contained (p1@(from1, to1), p2@(from2, to2)) =
    (from1 `within` p2 && to1 `within` p2) || (from2 `within` p1 && to2 `within` p1)


solveP2 :: [SecPair] -> Int
solveP2 = length.filter overlap


solveP1 :: [SecPair] -> Int
solveP1 = length.filter contained


secP :: Parser Sec
secP =
    do
        from <- intP
        to   <- char '-' >> intP
        return (from, to)


secPairP :: Parser SecPair
secPairP =
    do
        sec1 <- secP
        sec2 <- char ',' >> secP
        return (sec1, sec2)


secPairListP :: Parser [SecPair]
secPairListP = secPairP `sepEndBy1` newline


main :: IO ()
main = applyInput secPairListP solveP1 solveP2
