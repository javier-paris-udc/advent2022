module Main where


import           AoC                (applyInput, commaSepP, intP)
import           Data.HashSet       (HashSet)
import qualified Data.HashSet       as Set
import           Data.List          (foldl')
import           Data.Tuple.Extra   (first3, fst3, second3, snd3, third3, thd3)
import           Text.Parsec        (sepEndBy, spaces)
import           Text.Parsec.String (Parser)

type Cube  = (Int, Int, Int)
type Cubes = HashSet Cube
type Bound = (Int, Int, Int)



movesFrom :: Cube -> Cubes -> Cubes -> Bound -> Bound -> [Cube]
movesFrom c cubes stm maxCoords minCoords =
    filter steamCanMove (sides c)
  where
    notMember e    = not . Set.member e
    steamCanMove c = c `notMember` stm
                  && c `notMember` cubes
                  && inside c
    inside (x,y,z) = x >= fst3 minCoords && x <= fst3 maxCoords
                  && y >= snd3 minCoords && y <= snd3 maxCoords
                  && z >= thd3 minCoords && z <= thd3 maxCoords


insertSet :: Cubes -> [Cube] -> Cubes
insertSet = foldl' (flip Set.insert)


grow :: Cubes -> [Cube] -> Cubes -> Bound -> Bound -> Cubes
grow stm front cubes maxCoords minCoords =
    case front of
        []   -> stm
        p:ps -> let next = movesFrom p cubes stm maxCoords minCoords
                in grow (insertSet stm next) (next++ps) cubes maxCoords minCoords


steam :: Cubes -> Cubes
steam cubes = grow (Set.singleton (0,0,0)) [(0,0,0)] cubes maxCoords minCoords
  where
    maxCoord coord = maximum (Set.map coord cubes) + 1
    minCoord coord = minimum (Set.map coord cubes) - 1
    maxCoords = (maxCoord fst3, maxCoord snd3, maxCoord thd3)
    minCoords = (minCoord fst3, minCoord snd3, minCoord thd3)


sides :: Cube -> [Cube]
sides c = [first3, second3, third3] <*> [(+1), subtract 1] <*> [c]


sidesIn :: Cubes -> Cube -> Int
sidesIn cubes = length . filter (`Set.member` cubes) . sides


sidesNotIn :: Cubes -> Cube -> Int
sidesNotIn cubes = (6-) . sidesIn cubes


solveP2 :: Cubes -> Int
solveP2 cubes = sum $ map (sidesIn (steam cubes)) (Set.toList cubes)


solveP1 :: Cubes -> Int
solveP1 cubes = sum $ map (sidesNotIn cubes) (Set.toList cubes)


cubesP :: Parser Cubes
cubesP = Set.fromList <$> cubeP `sepEndBy` spaces
  where
    cubeP = do
        x <- intP
        commaSepP
        y <- intP
        commaSepP
        z <- intP
        return (x, y, z)


main :: IO ()
main = applyInput cubesP solveP1 solveP2