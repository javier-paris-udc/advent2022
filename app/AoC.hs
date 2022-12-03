module AoC where

import Text.Parsec (parse)
import Text.Parsec.String (Parser)
import System.Environment (getArgs, getProgName)

applyInput :: Show b => Parser a -> (a -> b) -> (a -> b) -> IO ()
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