{-# LANGUAGE NumericUnderscores #-}
module Main where

import           Text.Parsec ((<|>)
                             ,Parsec
                             ,char
                             ,getState
                             ,many1
                             ,modifyState
                             ,newline
                             ,putState
                             ,satisfy
                             ,sepEndBy1
                             ,spaces
                             ,string)
import           AoC         (applyInput, blanksP, intP)
import           Data.Map    (Map, (!))
import qualified Data.Map    as Map
import           Data.Char   (isSpace)
import           Data.Maybe  (catMaybes)


data Entry =
     Dir  String
    |File String Int
    deriving (Show, Eq)

type Path = [String]  -- Path as a list of String, with the root at the last position


dirSize :: Map Path [Entry] -> Path -> Int
dirSize m path = sum $ map (entrySize m path) (m ! path)


entrySize :: Map Path [Entry] -> Path -> Entry -> Int
entrySize m path entry =
    case entry of
        File _    size -> size
        Dir  name      -> dirSize m (name:path)


solveP2 :: Map Path [Entry] -> Int
solveP2 m = minimum $ Map.filter (>= toFree) $ Map.mapWithKey (const.dirSize m) m
  where
    freeSpace = 70_000_000 - dirSize m ["/"]
    toFree    = 30_000_000 - freeSpace


solveP1 :: Map Path [Entry] -> Int
solveP1 m = sum $ Map.filter (<=100_000) $ Map.mapWithKey (const.dirSize m) m


nameP :: Parsec String Path String
nameP = many1 (satisfy (not.isSpace))


entryP :: Parsec String Path Entry
entryP =
    dirP <|> fileP
  where
    dirP  = string "dir" >> blanksP >> Dir <$> nameP
    fileP = do
        size <- intP
        blanksP
        name <- nameP
        return $ File name size


lsP :: Parsec String Path (Path, [Entry])
lsP = do
    string "ls"
    newline
    cwd     <- getState
    entries <- entryP `sepEndBy1` newline
    return (cwd, entries)


cdP :: Parsec String Path ()
cdP = do
    string "cd"
    blanksP
    name <- nameP
    spaces
    case name of
        "/"  -> putState    ["/"]
        ".." -> modifyState tail
        _    -> modifyState (name:)


commandP :: Parsec String Path (Maybe (Path, [Entry]))
commandP = do
    char '$'
    blanksP
    (cdP >> return Nothing) <|> (Just <$> lsP)


entryMapP :: Parsec String Path (Map Path [Entry])
entryMapP = do
    entryList <- many1 commandP
    return $ Map.fromList (catMaybes entryList)


main :: IO ()
main = applyInput entryMapP ["/"] solveP1 solveP2