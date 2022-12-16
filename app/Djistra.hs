module Djistra (djistra) where

import           Data.Bifunctor (bimap)
import           Data.List      (foldl')
import           Data.Sequence  (Seq, ViewL ((:<)))
import qualified Data.Sequence  as Seq
import qualified Data.Set       as Set


binInsert :: (Ord b) => Seq ([a], b) -> ([a], b) -> Seq ([a], b)
binInsert seq x@(path, cost) =
    Seq.insertAt pos x seq
  where
    pos = findPos 0 (Seq.length seq)
    findPos left right
        | left == right                   = left
        | snd (Seq.index seq mid) == cost = mid
        | snd (Seq.index seq mid) <  cost = findPos (mid + 1) right
        | otherwise                       = findPos left (mid - 1)
      where
        mid = (left + right) `div` 2


djistra :: (Eq a, Ord a, Ord b, Num b)
        => a
        -> (a -> Bool)
        -> (a -> [(a, b)])
        -> Maybe [a]
djistra start isEnd adj =
    runDjistra (Seq.singleton ([start], 0)) (Set.singleton start)
  where
    runDjistra next visited = case Seq.viewl next of
        (path@(pos : _), cost) :< rest
            | isEnd pos -> Just (reverse path)
            | otherwise -> let newVertex  = expandPath visited pos
                               newPaths   = bimap (:path) (+cost) <$> newVertex
                               newVisited = Set.insert pos visited
                               nextNoPos  = Seq.filter ((/= pos) . head . fst) next
                               newNext    = foldl' binInsert nextNoPos newPaths
                           in runDjistra newNext newVisited
        Seq.EmptyL      -> Nothing
    expandPath visited pos = filter ((`Set.notMember` visited) . fst) $ adj pos

