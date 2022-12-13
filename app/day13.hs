module Main where

import AoC                (applyInput, commaSepP, intP)
import Control.Arrow      ((>>>))
import Data.List          (findIndices, sort, unfoldr)
import Text.Parsec        ((<|>), between, char, many, newline, sepBy, sepEndBy, spaces)
import Text.Parsec.String (Parser)


data Packet = I Int | L [Packet] deriving (Show, Eq)

instance Ord Packet where
    I x <= I y  = x <= y
    L x <= L y  = x <= y
    x   <= I y  = x <= L [I y]
    x   <= y    = L [x] <= y


groupsOf :: Int -> [a] -> [[a]]
groupsOf n = unfoldr (\l -> if null l then Nothing else Just (splitAt n l))


solveP2 :: [Packet] -> Int
solveP2 = ([divider2, divider6] ++)
      >>> sort
      >>> findIndices (\x -> x == divider2 || x == divider6)
      >>> map (+1)
      >>> product
  where
    divider2 = L [ L [ I 2]]
    divider6 = L [ L [ I 6]]


solveP1 :: [Packet] -> Int
solveP1 = groupsOf 2
      >>> findIndices isMonotonic
      >>> map (+1)
      >>> sum
  where
    isMonotonic l = and $ zipWith (<=) l (drop 1 l)



-- Parser


listP :: Parser Packet
listP = do
    packets <- between (char '[') (char ']') $ packetP `sepBy` commaSepP
    return $ L packets
  where
    packetP = listP <|> I <$> intP


packetsP :: Parser [Packet]
packetsP = listP `sepEndBy` spaces


main :: IO ()
main = applyInput packetsP solveP1 solveP2