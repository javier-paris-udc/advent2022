{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Main where


import AoC (applyInput, commaSepP, intP)
import Text.Parsec.String (Parser)
import Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as Map
import Text.Parsec (sepBy, spaces, string, many1, upper, (<|>), try, sepEndBy)
import Data.List ((\\), foldl')
import Data.Sequence (Seq ((:<|)), ViewL ((:<)), (><))
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Bifunctor (bimap)
import Data.Set (Set)
import Data.Function (on)


data Valve  = Valve {flow :: Int, tunnels :: HashMap String Int} deriving (Show, Eq)

data Path = Path {last :: String, path :: Set String, time :: Int, totalFlow :: Int}
    deriving (Show, Eq)


search :: HashMap String Valve
       -> Seq Path
       -> HashMap (String, Set String) (Int, Int)
       -> Path
       -> Int
search m paths visited optimum =
    case paths of
        Seq.Empty -> optimum.totalFlow
        p:<|ps    ->
            let dsts     = filter (checkPath p) $ Map.toList $ (m ! p.last).tunnels
                newPaths = filter checkVisited $ addDst p <$>  dsts
            in case newPaths of
                [] -> search m ps visited (maxFlow optimum p)
                _  -> search m
                            (Seq.fromList newPaths >< ps)
                            (foldl' addPath visited newPaths)
                            optimum
  where
    addDst p (dst, cost) =
        let tLeft = p.time - cost - 1
        in Path {last = dst
                ,path = Set.insert dst p.path
                ,time = tLeft
                ,totalFlow = p.totalFlow + tLeft * (m ! dst).flow}
    checkPath p (dst, cost) =
        p.time - cost - 1 >= 0 &&
        Set.notMember dst p.path
    checkVisited (p:: Path) =
        case Map.lookup (p.last, p.path) visited of
            Nothing -> True
            Just (timeLeft, flow1)
                | timeLeft <= p.time && flow1 >= p.totalFlow -> False
                | otherwise                                  -> True
    addPath visited (p::Path) = Map.insert (p.last, p.path) (p.time, p.totalFlow) visited
    maxFlow (p1::Path) (p2 :: Path)
        | p1.totalFlow >= p2.totalFlow = p1
        | otherwise                    = p2


maxPresRelief :: HashMap String Valve -> Int
maxPresRelief m = search m (Seq.singleton path0) (Map.singleton (path0.last, path0.path) (path0.time, path0.totalFlow)) path0
  where
    path0 = Path {last = "AA", path = Set.singleton "AA", time = 30, totalFlow = 0}


findShortest :: HashMap String Valve -> String -> String -> Int
findShortest m v1 v2 = findAux [[v1]] [] [v1]
  where
    findAux []    []   visited = undefined
    findAux []  nextL  visited = findAux nextL [] visited
    findAux (path@(p:ps):paths) nextL visited
        | p == v2   = length path - 1
        | otherwise =
            let dsts = filter (not . (`elem` visited)) $ Map.keys (m ! p).tunnels
                newPaths = (:path) <$> dsts
            in findAux paths (newPaths ++ nextL) (dsts ++ visited)



simplifyValve :: HashMap String Valve -> [String] -> String -> Valve
simplifyValve m valves v =
    Valve {flow    = flow (m ! v),
           tunnels = Map.fromList  $ map (\to -> (to, findShortest m v to)) (valves \\ [v])
          }


simplifyMap :: HashMap String Valve -> HashMap String Valve
simplifyMap m = Map.fromList simplifyed
  where
    flowValves = "AA" : Map.keys (Map.filter ((>0) . flow) m)
    simplifyed = map (\v -> (v, simplifyValve m flowValves v)) flowValves


solveP2 :: HashMap String Valve -> Int
solveP2 = undefined


solveP1 :: HashMap String Valve -> Int
solveP1 = maxPresRelief . simplifyMap



valvesP :: Parser (HashMap String Valve)
valvesP = Map.fromList <$> (valveP `sepEndBy` spaces)
  where
    valveP = do
        string "Valve" >> spaces
        name <- valveNameP
        spaces
        string "has flow rate="
        flow <- intP
        try (string "; tunnels lead to valves") <|> string "; tunnel leads to valve"
        spaces
        tunnels <- Map.fromList . map (, 1) <$> valveNameP `sepBy` commaSepP
        return (name, Valve {..})
    valveNameP = many1 upper


main :: IO ()
main = applyInput valvesP solveP1 (const 0)