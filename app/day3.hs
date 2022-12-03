module Main where
import System.Environment (getArgs, getProgName)
import Text.Parsec (
     parse
    ,newline
    ,many1
    ,letter
    ,sepEndBy1)
import Text.Parsec.String (Parser)
import Data.Char (isLower, ord)
import Data.List (intersect)


priority :: Char -> Int
priority c
    | isLower c = ord c - ord 'a' + 1
    | otherwise = ord c - ord 'A' + 27


badges :: [String] -> [Char]
badges (s1:s2:s3:ss) = head (s1 `intersect` s2 `intersect` s3):badges ss
badges            [] = []


solveP2 :: [String] -> Int
solveP2 = sum . map priority . badges


solveP1 :: [String] -> Int
solveP1 =
    sum . map (priority.findDup)
  where
    findDup s = head $ uncurry intersect $ splitAt (length s `div` 2) s


backPacksP :: Parser [String]
backPacksP = many1 letter `sepEndBy1` newline


main :: IO ()
main =
    do
        args <- getArgs
        prog <- getProgName
        case args of
            [inputFile] ->
                do
                    input <- readFile inputFile
                    case parse backPacksP inputFile input of
                        Left err -> print err
                        Right backPacks ->
                            do
                                print $ solveP1 backPacks
                                print $ solveP2 backPacks
            _ -> putStrLn $ "Use: "++prog++" input"