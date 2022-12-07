module AoC where

import Text.Parsec        (char
                          ,(<|>)
                          ,many1
                          ,digit
                          ,option
                          ,oneOf
                          ,ParseError
                          ,Parsec, runParser)
import Text.Parsec.Pos    (SourceName)
import Text.Parsec.String (parseFromFile)
import System.Environment (getArgs, getProgName)
import Control.Monad      (void)


intP :: Parsec String a Int
intP =
    do
        sign <- option 1 (char '-' >> return (-1))
        num  <- read <$> many1 digit
        return (num * sign)


blankP :: Parsec String a ()
blankP = void $ oneOf " \t"


blanksP :: Parsec String a ()
blanksP = void $ many1 blankP


applyInput :: (Show b, Show c) => Parsec String s a -> s -> (a -> b) -> (a -> c) -> IO ()
applyInput parser state solveP1 solveP2 =
    do
        args <- getArgs
        prog <- getProgName
        case args of
            [inputFile] ->
                do
                    fileContent <- readFile inputFile
                    case runParser parser state inputFile fileContent of
                        Left err ->
                            print err
                        Right parsedRes ->
                            do
                                print $ solveP1 parsedRes
                                print $ solveP2 parsedRes
            _ -> print $ "Use: "++prog++" input"