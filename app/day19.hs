{-# LANGUAGE DeriveGeneric #-}
module Main where

import           AoC                 (applyInput, intP)
import           Control.Arrow       ((>>>))
import           Data.Function       ((&))
import           Data.Hashable       (Hashable)
import qualified Data.HashMap.Strict as Map
import           Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashSet        as Set
import           Data.HashSet        (HashSet)
import           Text.Parsec         (char
                                     ,choice
                                     ,many
                                     ,string
                                     ,sepBy
                                     ,sepEndBy
                                     ,spaces
                                     ,try)
import           Text.Parsec.String  (Parser)
import           GHC.Generics        (Generic)


data Resource  = Ore | Clay | Obsidian | Geode deriving (Show, Eq, Generic)

instance Hashable Resource

type BuildCost = [(Resource, Int)]

data BluePrint = BluePrint {num       :: Int
                           ,robotCost :: HashMap Resource BuildCost
                           } deriving (Show, Eq)

type ResourceMap = HashMap Resource Int
type RobotMap    = HashMap Resource Int


extract :: Int -> RobotMap -> ResourceMap -> ResourceMap
extract time robots = Map.mapWithKey (\r -> (+ time * robots ! r))


advanceTime :: Int
            -> RobotMap
            -> [(Resource, (Int, BuildCost))]
            -> ResourceMap
            -> [(RobotMap, ResourceMap, Int)]
advanceTime now robots newRobots resources =
    map addTime newRobots
  where
    addTime (res, (time, buildCost)) =
        (Map.adjust (+1) res robots
        ,foldr (\(res, cost) -> Map.adjust (subtract cost) res)
                                           (extract time robots resources)
                                           buildCost
        ,now - time
        )


buildable :: HashMap Resource Int
          -> BluePrint
          -> RobotMap
          -> ResourceMap
          -> Int
          -> Int
          -> [(Resource, (Int, BuildCost))]
buildable maxResources bluePrint robots resources time maxGeodes =
      buildableRobots
    & fmap (\r -> (timeTo r, r))
    & Map.filterWithKey (\res (timeCost, _) -> timeCost <= time
                                            && resources ! Geode +
                                               robots ! Geode * time +
                                               sumIntTo (time-timeCost)> maxGeodes
                                            && notMaxRobots res (robots ! res))
    & Map.toList
  where
    produced             = (> 0).(robots !).fst
    buildableRobots      = Map.filter (all produced) $ robotCost bluePrint
    timeTo               = map (\(r, amount) -> (amount - (resources ! r))
                                                 `divUp` (robots ! r))
                       >>> maximum
                       >>> (+1)
    divUp x y            = max 0 $ x `div` y + (if x `rem` y > 0 then 1 else 0)
    sumIntTo n           = n * (n + 1) `div` 2
    notMaxRobots res cur =
        case maxResources Map.!? res of
            Nothing -> True
            Just x  -> cur < x


insert :: Hashable a => [a] -> HashSet a -> HashSet a
insert = flip (foldr Set.insert)


search :: HashMap Resource Int
       ->[(RobotMap, ResourceMap, Int)]
       -> HashSet (RobotMap, ResourceMap, Int)
       -> Int
       -> BluePrint
       -> Int
search maxResources paths visited maxGeodes bluePrint =
    case paths of
        []                           -> maxGeodes
        (robots, resources, 0):ps    ->
            search maxResources ps visited (max maxGeodes (resources ! Geode)) bluePrint
        (robots, resources, time):ps ->
            case buildable maxResources bluePrint robots resources time maxGeodes of
                [] ->
                    let newResources = extract time robots resources
                    in search maxResources
                              ps
                              visited
                              (max maxGeodes (newResources ! Geode))
                              bluePrint
                newRobots ->
                    let newPaths = filter (not. (`Set.member` visited)) $
                                    advanceTime time robots newRobots resources
                    in search maxResources
                              (newPaths ++ ps)
                              (insert newPaths visited)
                              maxGeodes
                              bluePrint


quality :: Int -> BluePrint -> Int
quality n b = search maxResources [initPath] (Set.singleton initPath) 0 b
  where
    robots       = Map.fromList [(Ore, 1), (Clay, 0), (Obsidian, 0), (Geode, 0)]
    resources    = Map.fromList [(Ore, 0), (Clay, 0), (Obsidian, 0), (Geode, 0)]
    maxResources = Map.fromList $ (\res -> (res, bpResReq res)) <$> [Ore, Clay, Obsidian]
    initPath     = (robots, resources, n)
    bpResReq res = maximum $ map snd $ concatMap (filter ((==res).fst)) (robotCost b)

solveP2 :: [BluePrint] -> Int
solveP2 = product . map (quality 32). take 3


solveP1 :: [BluePrint] -> Int
solveP1 = sum . map (\b -> num b * quality 24 b)


bluePrintsP :: Parser [BluePrint]
bluePrintsP = many bluePrintP
  where
    bluePrintP = do
        string "Blueprint "
        id <- intP
        char ':'
        spaces
        reqs <- resourceReqP `sepEndBy` (char '.' >> spaces)
        return BluePrint {num = id, robotCost = Map.fromList reqs}

    resourceReqP = do
        string "Each "
        resource <- resourceP
        string " robot costs "
        reqs <- reqP `sepBy` try (string " and ")
        return (resource, reqs)

    resourceP = choice [try (string "ore")      >> return Ore
                       ,try (string "clay")     >> return Clay
                       ,try (string "obsidian") >> return Obsidian
                       ,try (string "geode")    >> return Geode
                       ]
    reqP = do
        amount <- intP
        spaces
        res <- resourceP
        return (res, amount)



main :: IO ()
main = applyInput bluePrintsP solveP1 solveP2