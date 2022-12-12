module AoC where

import Text.Parsec        (char
                          ,(<|>)
                          ,many1
                          ,digit
                          ,option
                          ,oneOf
                          ,ParseError
                          ,Parsec
                          ,runParser
                          ,spaces
                          ,try)
import Text.Parsec.Pos    (SourceName)
import Text.Parsec.String (Parser)
import System.Environment (getArgs, getProgName)
import Control.Monad      (void)
import Data.Bifunctor (first)


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


commaSepP :: Parsec String a ()
commaSepP = try $ spaces >> char ',' >> spaces


getParsedInput :: Parsec String s a -> s -> String -> IO a
getParsedInput parser state file = do
    fileContents <- readFile file
    let Right parseRes = runParser parser state file fileContents
    return parseRes


parseFromArg :: Parsec String s a -> s -> IO (Either String a)
parseFromArg parser state = do
    args <- getArgs
    prog <- getProgName
    case args of
        [inputFile] ->
            do
                fileContent <- readFile inputFile
                return $ first show $ runParser parser state inputFile fileContent
        _ ->
            return $ Left $ "Use: "++prog++" input"


applyInputSWith :: Parsec String s a
                -> s
                -> (a -> b)
                -> (a -> c)
                -> (b -> IO ())
                -> (c -> IO ())
                -> IO ()
applyInputSWith parser state solveP1 solveP2 printP1 printP2 =
    do
        parseRes <- parseFromArg parser state
        case parseRes of
            Left err ->
                print err
            Right parsedRes ->
                do
                    printP1 $ solveP1 parsedRes
                    printP2 $ solveP2 parsedRes


applyInput :: (Show b, Show c) => Parser a -> (a -> b) -> (a -> c) -> IO ()
applyInput = flip applyInputS ()


applyInputS :: (Show b, Show c) => Parsec String s a -> s -> (a -> b) -> (a -> c) -> IO ()
applyInputS parser state solveP1 solveP2 =
    applyInputSWith parser state solveP1 solveP2 print print
