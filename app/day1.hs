module Main where
import Text.Parsec (
     many1
    ,digit
    ,newline
    ,sepEndBy1)
import Text.Parsec.String (Parser)
import AoC (applyInput)
import Data.List (sortBy)
import Control.Arrow ((>>>))


solveP2 :: [[Int]] -> Int
solveP2 = fmap sum >>> sortBy (flip compare) >>> take 3 >>> sum


solveP1 :: [[Int]] -> Int
solveP1 = fmap sum >>> maximum


elfsP :: Parser [[Int]]
elfsP = elfP `sepEndBy1` newline
  where
    elfP = intP `sepEndBy1` newline
    intP = read <$> many1 digit


main :: IO ()
main = applyInput elfsP solveP1 solveP2