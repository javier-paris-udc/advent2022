{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Main where


import           AoC                 (applyInput, commaSepP, intP)
import           Data.Bifunctor      (bimap)
import           Data.Foldable       (minimumBy)
import           Data.Function       (on)
import           Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as Map
import           Data.List           ((\\), foldl', sort)
import           Data.Sequence       (Seq ((:<|)), ViewL ((:<)), (><))
import qualified Data.Sequence       as Seq
import           Data.Set            (Set)
import qualified Data.Set            as Set
import           Text.Parsec         (sepBy
                                     ,spaces
                                     ,string
                                     ,many1
                                     ,upper
                                     ,(<|>)
                                     ,try
                                     ,sepEndBy)
import           Text.Parsec.String (Parser)


data Valve = Valve {flow :: Int, tunnels :: HashMap String Int} deriving (Show, Eq)

data Path = Path {last :: HashMap Int String
                 ,path :: Set String
                 ,time :: HashMap Int Int
                 ,totalFlow :: Int}
            deriving (Show, Eq)


search :: HashMap String Valve
       -> Int
       -> Seq Path
       -> HashMap (Set String, Set String) (Set Int, Int)
       -> Path
       -> Int
search m n paths visited optimum =
    case paths of
        Seq.Empty -> optimum.totalFlow
        p:<|ps    ->
            let nexts    = Map.toList p.time
                alldsts  = map (filterCost p) nexts
                newPaths = concatMap (filterVisited p) alldsts
            in case newPaths of
                [] -> search m n ps visited (maxFlow optimum p)
                _  -> search m
                            n
                            (Seq.fromList newPaths >< ps)
                            (foldl' addPath visited newPaths)
                            optimum
  where
    filterCost p (nxt, tNxt) =
        ((nxt, tNxt),) <$> filter (checkPath p tNxt)
                        $ Map.toList
                        $ tunnels (m ! (p.last ! nxt))

    filterVisited p ((nxt, tNxt), dsts) = filter checkVisited $ addDst p nxt tNxt <$> dsts


    addDst p nxt t (dst, cost) =
        let tLeft = t - cost - 1
        in Path {last      = Map.insert nxt dst p.last
                ,path      = Set.insert dst p.path
                ,time      = Map.insert nxt tLeft p.time
                ,totalFlow = p.totalFlow + tLeft * (m ! dst).flow }

    checkPath p t (dst, cost) =
        t - cost - 1 >= 1 &&
        Set.notMember dst p.path

    checkVisited (p:: Path) =
        case Map.lookup (Set.fromList $ Map.elems p.last, p.path) visited of
            Nothing -> True
            Just (timeLeft, flow1)
                | and (zipWith (<=) (cycle $ Set.toAscList timeLeft)
                                    (sort $ Map.elems (p.time)))
                  && flow1 >= p.totalFlow -> False
                | otherwise               -> True

    addPath visited (p::Path) = Map.insert (Set.fromList $ Map.elems p.last, p.path)
                                           (Set.fromList $ Map.elems p.time, p.totalFlow)
                                           visited

    maxFlow (p1::Path) (p2 :: Path)
        | p1.totalFlow >= p2.totalFlow = p1
        | otherwise                    = p2


maxPresRelief :: Int -> Int -> HashMap String Valve -> Int
maxPresRelief n totalTime m =
    search m
           n
           (Seq.singleton path0)
           (Map.singleton (Set.singleton "AA", path0.path)
                          (Set.singleton totalTime, path0.totalFlow))
           path0
  where
    path0 = Path {last = Map.fromList $ zip [1..n] (repeat "AA")
                 ,path = Set.singleton "AA"
                 ,time = Map.fromList $ zip [1..n] (repeat totalTime)
                 ,totalFlow = 0}


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
solveP2 = maxPresRelief 2 26 . simplifyMap


solveP1 :: HashMap String Valve -> Int
solveP1 = maxPresRelief 1 30 . simplifyMap



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
main = applyInput valvesP solveP1 solveP2