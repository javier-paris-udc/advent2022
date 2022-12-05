module Main where
import AoC (applyInput, intP)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Text.Parsec.String (Parser)
import Text.Parsec (sepEndBy1, spaces, string, newline, sepBy1, char, (<|>), anyChar, try)
import Data.List (transpose, foldl')
import Data.Maybe (fromJust, isJust, catMaybes)


type Stack = Map Int [Char]
data Op = Op
    {getFrom  :: Int
    ,getTo    :: Int
    ,getCount :: Int
    } deriving (Show, Eq)

type StackSt = (Stack, [Op])


doOp :: ([Char] -> [Char]) -> Stack -> Op -> Stack
doOp craneOp st (Op from to count) =
    let removedCratesMap = Map.adjust (drop count) from st
        movedCrates      = craneOp $ take count $ st ! from
    in  Map.adjust (movedCrates++) to removedCratesMap


solveP2 :: StackSt -> String
solveP2 (st, ops) = map head $ Map.elems $ foldl' (doOp id) st ops


solveP1 :: StackSt -> String
solveP1 (st, ops) = map head $ Map.elems $ foldl' (doOp reverse) st ops


buildMap :: [Int] -> [[Char]] -> Map Int [Char]
buildMap keys values =
    foldl' (\m (k,v) -> Map.insert k v m) Map.empty (zip keys values)


elemP :: Parser (Maybe Char)
elemP = do
    emptyP <|> charP
  where
    emptyP = try (string "   ") >> return Nothing
    charP = do
        char '['; c <- anyChar; char ']'
        return $ Just c


stackP :: Parser Stack
stackP = do
    rows <- (elemP `sepBy1` char ' ') `sepEndBy1` newline
    let values = catMaybes <$> transpose rows
    spaces
    keys <- intP `sepBy1` try (string "   ")
    return $ buildMap keys values


opP :: Parser Op
opP = do
    string "move "
    count <- intP
    string " from "
    from   <- intP
    string " to "
    to     <- intP
    return (Op from to count)


stackStP :: Parser StackSt
stackStP = do
    stack <- stackP
    spaces
    ops <- opP `sepEndBy1` newline
    return (stack, ops)



main :: IO ()
main = applyInput stackStP solveP1 solveP2