module Main where


import           AoC                  (applyInput, sep, intP)
import           Control.Monad.Reader (Reader, runReader, asks)
import qualified Data.HashMap.Strict  as Map
import           Data.HashMap.Strict  (HashMap, (!))
import           Data.List            (unfoldr)
import           Text.Parsec          ((<|>)
                                      ,char
                                      ,choice
                                      ,letter
                                      ,many1
                                      ,newline
                                      ,sepEndBy
                                      ,spaces)
import           Text.Parsec.String   (Parser)


-- Types

data Op = Add | Sub | Mul | Div | Eql
    deriving (Show, Eq)


data Oper = One | Two
    deriving (Show, Eq)


data Expr = Val Int
          | Expr Op String String
    deriving (Show,Eq)


-- Eval functions

opToFun :: Op -> Int -> Int -> Int
opToFun op =
    case op of
        Add -> (+)
        Sub -> (-)
        Mul -> (*)
        Div -> div


eval :: String -> Reader (HashMap String Expr) Int
eval name = do
    expr <- asks (! name)
    case expr of
        Val x         -> return x
        Expr op v1 v2 -> do
            val1 <- eval v1
            val2 <- eval v2
            return $ opToFun op val1 val2



-- Estimate needed value for humn for part2

addRevVars :: HashMap String String -> String -> Expr -> HashMap String String
addRevVars m var val =
    case val of
        Expr op v1 v2 -> Map.insert v2 var $ Map.insert v1 var m
        _             -> m


computeVal :: Int -> Int -> Op -> Oper -> Int
computeVal total val1 op side =
    case op of
        Add               -> total - val1
        Mul               -> total `div` val1
        Sub | side == One -> total + val1
            | side == Two -> val1 - total
        Div | side == One -> total * val1
            | side == Two -> val1 `div` total
        Eql               -> val1


findHumnVal :: Int -> String -> [String] -> Reader (HashMap String Expr) Int
findHumnVal val _   []     = return val
findHumnVal val var (v:vs) = do
    expr <- asks (! var)
    case expr of
        Expr op v1 v2
            | v == v1 -> do
                valv2 <- eval v2
                findHumnVal (computeVal val valv2 op One) v1 vs
            | otherwise -> do
                valv1 <- eval v1
                findHumnVal (computeVal val valv1 op Two) v2 vs


-- Solvers

solveP2 :: HashMap String Expr -> Int
solveP2 m = runReader (findHumnVal 0 "root" pathToHumn) mWithRootEq
  where
    mWithRootEq   = Map.adjust (\(Expr _ v1 v2) -> Expr Eql v1 v2) "root" m
    reverseMap    = Map.foldlWithKey' addRevVars Map.empty mWithRootEq
    pathToHumn    = reverse $ unfoldr nextUp "humn"

    nextUp "root" = Nothing
    nextUp var    = Just (var, reverseMap ! var)


solveP1 :: HashMap String Expr -> Int
solveP1 = runReader (eval "root")


-- Parser

exprsP :: Parser (HashMap String Expr)
exprsP = Map.fromList <$> exprP `sepEndBy` newline
  where
    exprP = do
        defName <- identP
        sep ":"
        val <- (Val <$> intP) <|> arithP
        return (defName, val)

    identP = many1 letter

    arithP = do
        a <- identP
        spaces
        op <- opP
        spaces
        b <- identP
        return $ Expr op a b

    opP = choice [char '+' >> return Add,
                  char '*' >> return Mul,
                  char '-' >> return Sub,
                  char '/' >> return Div
                 ]


main :: IO ()
main = applyInput exprsP solveP1 solveP2