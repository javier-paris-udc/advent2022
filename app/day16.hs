{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Main where


import           AoC                 (applyInput, commaSepP, intP)
import           Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as Map
import           Data.List           ((\\), delete, foldl', sort, sortBy)
import           Data.Sequence       (Seq ((:<|)), (><))
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
import Data.Maybe (mapMaybe)


data Valve = Valve {flow :: Int, tunnels :: HashMap String Int} deriving (Show, Eq)

data Path = Path {last :: HashMap Int String
                 ,path :: Set String
                 ,time :: HashMap Int Int
                 ,totalFlow :: Int
                 ,potential :: [Int]}
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
                alldsts  = map (filterCost m p) nexts
                newPaths = concatMap (filterVisited m optimum visited p) alldsts
            in case newPaths of
                [] -> search m n ps visited (maxFlow optimum (p {potential = []}))
                _  -> search m
                            n
                            (Seq.fromList newPaths >< ps)
                            (foldl' addPath visited newPaths)
                            optimum


filterCost :: HashMap String Valve -> Path -> (Int, Int) -> ((Int,Int), [(String, Int)])
filterCost m p (nxt, tNxt) =
    ((nxt, tNxt),) <$> filter (checkPath p tNxt)
                    $ Map.toList
                    $ tunnels (m ! (p.last ! nxt))


filterVisited :: HashMap String Valve
              -> Path
              -> HashMap (Set String, Set String) (Set Int, Int)
              -> Path
              -> ((Int, Int) , [(String, Int)])
              -> [Path]
filterVisited m optimum visited p ((nxt, tNxt), dsts) =
    filter (checkVisited optimum visited) $ addDst m p nxt tNxt <$> dsts


addDst :: HashMap String Valve
       -> Path
       -> Int
       -> Int
       -> (String, Int)
       -> Path
addDst m p nxt t (dst, cost) =
    let tLeft   = t - cost - 1
        newPath = Set.insert dst p.path
        newTime = Map.insert nxt tLeft p.time
        newPotential = delete (m ! dst).flow p.potential
    in Path {last      = Map.insert nxt dst p.last
            ,path      = newPath
            ,time      = newTime
            ,totalFlow = p.totalFlow + tLeft * (m ! dst).flow
            ,potential = newPotential }


checkPath :: Path -> Int -> (String, Int) -> Bool
checkPath p t (dst, cost) =
    t - cost - 1 >= 1 &&
    Set.notMember dst p.path


estimatePotential :: [Int] -> [Int] -> Int
estimatePotential timesLeft potentials =
    let times = merge (mapMaybe (\n -> if n >= 3
                                       then Just [n - 2, n - 4 .. 1]
                                       else Nothing) timesLeft)
    in sum $ zipWith (*) potentials times


merge :: Ord a => [[a]] -> [a]
merge []     = []
merge (l:ls) =
    let (maxList, rest) = foldl' (\(maxl@(max1:_), acc) newl@(n2:_) ->
            if max1 > n2 then (maxl, newl:acc) else (newl, maxl:acc)) (l, []) ls
    in case tail maxList of
        []   -> head maxList : merge rest
        tmax -> head maxList : merge (tmax : rest)


checkVisited :: Path -> HashMap (Set String, Set String) (Set Int, Int) -> Path -> Bool
checkVisited optimum visited p
    |   estimatePotential (Map.elems p.time) p.potential
      + p.totalFlow <= optimum.totalFlow = False
    | otherwise =
        case Map.lookup (Set.fromList $ Map.elems p.last, p.path) visited of
            Nothing -> True
            Just (timeLeft, flow1)
                | and (zipWith (<=) (cycle $ Set.toAscList timeLeft)
                                    (sort $ Map.elems (p.time)))
                && flow1 >= p.totalFlow -> False
                | otherwise             -> True


addPath :: HashMap (Set String, Set String) (Set Int, Int)
        -> Path
        -> HashMap (Set String, Set String) (Set Int, Int)
addPath vstd (p::Path) = Map.insert (Set.fromList $ Map.elems p.last, p.path)
                                        (Set.fromList $ Map.elems p.time, p.totalFlow)
                                        vstd


maxFlow :: Path -> Path -> Path
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
           path0 {potential = []}
  where
    path0 = Path {last      = Map.fromList $ zip [1..n] (repeat "AA")
                 ,path      = Set.singleton "AA"
                 ,time      = Map.fromList $ zip [1..n] (repeat totalTime)
                 ,totalFlow = 0
                 ,potential = maxPotential}
    maxPotential = sortBy (flip compare) $ Map.elems (fmap flow m)


findShortest :: HashMap String Valve -> String -> String -> Int
findShortest m v1 v2 = findAux [[v1]] [] [v1]
  where
    findAux []        []        _ = undefined
    findAux []     nextL  visited = findAux nextL [] visited
    findAux ([]:_)     _        _ = undefined
    findAux (path@(p:_):paths) nextL visited
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
        _    <- string "has flow rate="
        flow <- intP
        _    <- try (   string "; tunnels lead to valves")
                    <|> string "; tunnel leads to valve"
        spaces
        tunnels <- Map.fromList . map (, 1) <$> valveNameP `sepBy` commaSepP
        return (name, Valve {..})
    valveNameP = many1 upper


main :: IO ()
main = applyInput valvesP solveP1 solveP2