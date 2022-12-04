module AoC where

import Text.Parsec (parse, char, (<|>), many1, digit, option)
import Text.Parsec.String (Parser)
import System.Environment (getArgs, getProgName)


intP :: Parser Int
intP =
    do
        sign <- option 1 (char '-' >> return (-1))
        num  <- read <$> many1 digit
        return (num * sign)


applyInput :: (Show b, Show c) => Parser a -> (a -> b) -> (a -> c) -> IO ()
applyInput parser solveP1 solveP2 =
    do
        args <- getArgs
        prog <- getProgName
        case args of
            [inputFile] ->
                do
                    input <- readFile inputFile
                    case parse parser inputFile input of
                        Left err ->
                            print err
                        Right parsedRes ->
                            do
                                print $ solveP1 parsedRes
                                print $ solveP2 parsedRes
            _ -> print $ "Use: "++prog++" input"