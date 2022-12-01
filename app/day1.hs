module Main where
import System.Environment (getArgs, getProgName)
import Text.Parsec (
     parse
    ,many1
    ,digit
    ,sepBy1
    ,newline
    ,notFollowedBy
    ,eof
    ,sepEndBy1)
import Text.Parsec.String (Parser)
import Data.List (sortBy)
import Control.Arrow ((>>>))


solveP2 :: [[Int]] -> Int
solveP2 = fmap sum >>> sortBy (flip compare) >>> take 3 >>> sum


solveP1 :: [[Int]] -> Int
solveP1 = fmap sum >>> maximum


elfsP :: Parser [[Int]]
elfsP = elfP `sepBy1` newline
  where
    elfP = intP `sepEndBy1` newline
    intP = read <$> many1 digit


main :: IO ()
main =
    do
        args <- getArgs
        prog <- getProgName
        case args of
            [inputFile] ->
                do
                    input <- readFile inputFile
                    case parse elfsP  "" input of
                        Left err -> print err
                        Right elfs ->
                            do
                                print $ solveP1 elfs
                                print $ solveP2 elfs
            _ -> putStrLn $ "Use: "++prog++" input"