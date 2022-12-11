{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}

module Main where


import           AoC                      (applyInput, blanksP, intP, commaSepP)
import           Control.Arrow            ((>>>))
import           Control.Monad            (forM_, replicateM_)
import           Control.Monad.State.Lazy (State, execState, gets, modify)
import qualified Data.IntMap              as Map
import           Data.IntMap              (IntMap, (!))
import           Data.List                (sortBy)
import           Data.Maybe               (fromMaybe)
import           Text.Parsec              (spaces
                                          ,string
                                          ,char
                                          ,(<|>)
                                          ,sepBy
                                          ,sepEndBy)
import           Text.Parsec.String       (Parser)


data Monkey = Monkey { items    :: [Int]
                     , op       :: Int -> Int
                     , test     :: Int -> Int
                     , factor   :: Int
                     , activity :: Int
                     }


-- Round runner

giveItem :: (Int -> Int) -> Int -> State (IntMap Monkey) ()
giveItem test worry =
    modify $ Map.adjust (addItem worry) (test worry)
  where
    addItem i Monkey { .. } = Monkey { items = worry : items, .. }


monkeyRound :: (Int -> Int) -> Int -> State (IntMap Monkey) ()
monkeyRound worryFun i = do
    Monkey {..} <- gets (! i)

    let worryLevels = worryFun . op <$> items
        newActivity = activity + length worryLevels

    modify $ Map.insert i (Monkey { items = [], activity = newActivity, .. })
    forM_ worryLevels (giveItem test)


roundSt :: (Int -> Int) -> State (IntMap Monkey) ()
roundSt worryFun = do
    keys <- gets Map.keys
    forM_ keys (monkeyRound worryFun)


solve :: (Int -> Int) -> Int -> IntMap Monkey -> Int
solve worryFun rounds =
        execState (replicateM_ rounds (roundSt worryFun))
    >>> Map.elems
    >>> map activity
    >>> sortBy (flip compare)
    >>> take 2
    >>> product


solveP2 :: IntMap Monkey -> Int
solveP2 m = solve worryFun 10_000 m
  where
    zfactor  = product $ fmap factor m
    worryFun = (`mod` zfactor)


solveP1 :: IntMap Monkey -> Int
solveP1 = solve (`div` 3) 20


-- Parsing

itemsP :: Parser [Int]
itemsP = do
    string "Starting items: "
    intP `sepEndBy` commaSepP


operationP :: Parser (Int -> Int)
operationP = do
    string "Operation: new = old "
    op <- charToOp <$> (char '*' <|> char '+')

    blanksP
    operand <- (string "old" >> return Nothing) <|> (Just <$> intP)

    return $ maybe (op <*> id) op operand
  where
    charToOp '+' = (+)
    charToOp '*' = (*)


testP :: Parser (Int, Int -> Int)
testP = do
    string "Test: divisible by "
    zmod <- intP
    spaces

    string "If true: throw to monkey "
    monkeyT <- intP
    spaces

    string "If false: throw to monkey "
    monkeyF <- intP

    return (zmod, test zmod monkeyT monkeyF)
  where
    test zmod monkeyT monkeyF x
        | x `mod` zmod == 0  = monkeyT
        | otherwise          = monkeyF


monkeyP :: Parser (Int, Monkey)
monkeyP = do
    string "Monkey "
    idx <- intP
    string ":"
    spaces

    items <- itemsP
    spaces

    op <- operationP
    spaces

    (factor, test) <- testP

    return (idx, Monkey {activity = 0, .. })


monkeysP :: Parser (IntMap Monkey)
monkeysP = Map.fromList <$> monkeyP `sepEndBy` spaces


main :: IO ()
main = applyInput monkeysP solveP1 solveP2