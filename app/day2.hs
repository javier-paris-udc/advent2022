module Main where

import Text.Parsec        (newline
                          ,(<|>)
                          ,char
                          ,space
                          ,sepEndBy1)
import Text.Parsec.String (Parser)
import AoC                (applyInput)

type Play = (Int, Int)


win :: Int -> Int -> Bool
win x y = x == (y+1) `mod` 3


tie :: Int -> Int -> Bool
tie = (==)


scoreRound :: Play -> Int
scoreRound (x,y)
    | win y x   = 6
    | tie y x   = 3
    | otherwise = 0


getDesiredOutcome :: Play -> Play
getDesiredOutcome (x, des)
    | des == 0  = (x, (x + 2) `mod` 3)
    | des == 1  = (x, x)
    | des == 2  = (x, (x + 1) `mod` 3)


solveP2 :: [Play] -> Int
solveP2 p = solveP1 plays
  where
    plays = getDesiredOutcome <$> p


solveP1 :: [Play] -> Int
solveP1 p = sum (scoreRound <$> p) + sum ((+1).snd <$> p)


gameP :: Parser [Play]
gameP = playP `sepEndBy1` newline
  where
    playP     = do {p1 <- rpsP; space; p2 <- rpsP; return (p1, p2) }
    rpsP      = rockP <|> paperP <|> scissorsP
    rockP     = (char 'A' <|> char 'X') >> return 0
    paperP    = (char 'B' <|> char 'Y') >> return 1
    scissorsP = (char 'C' <|> char 'Z') >> return 2


main :: IO ()
main = applyInput gameP solveP1 solveP2