import Data.List
import Data.Ord
import System.Random

type Population = [Chromosome]
type Chromosome = String
type Fitness = Int
type ChromosomeF = (Chromosome, Fitness)

inputData :: [(Int, Int)]
inputData = [(15, 15), (3, 7), (2, 10), (5, 5), (9, 8), (20, 17)]

baseCase :: Population
baseCase = ["100110", "001110", "010100", "011001"]

runGA :: Population -> Int -> Float -> ChromosomeF
runGA xs count mutationP = runGA' baseCase count 0 mutationP ("", 0)

runGA' :: Population -> Int -> Int -> Float -> ChromosomeF -> ChromosomeF
runGA' xs count curTimes mutationP (chro, fit) | curTimes < count = runGA' newPopulation count (curTimes + 1) mutationP best
                                               | otherwise = best
                                               where newPopulation = runGA'' xs curTimes mutationP
                                                     chroFlist = buildPopulation newPopulation 30 inputData
                                                     [(chro2, fit2)] | not (null chroFlist) = take 1 (sortPopulation chroFlist)
                                                                     | otherwise = [(chro, fit)]
                                                     best | fit2 > fit = (chro2, fit2)
                                                          | otherwise = (chro, fit)

runGA'' :: Population -> Int -> Float -> Population
runGA'' xs count mutationP = mutationAll (crossoverAll (selectParents (length xs) count 2 (length xs) (buildPopulation' xs 30 inputData))) mutationP count

crossoverAll :: [(Chromosome, Chromosome)] -> Population
crossoverAll [] = []
crossoverAll (x:xs) = let (a, b) = crossover x
                      in a : b : crossoverAll xs

mutationAll :: Population -> Float -> Int -> Population
mutationAll [] _ _ = []
mutationAll (x:xs) percent seed = mutation x percent seed : mutationAll xs percent seed

--------------- TEST
-- >>> runGA baseCase 1 0.6
-- ("011110",30)

-- >>> runGA baseCase 3 0.6
-- ("011110",30)

-- >>> runGA baseCase 5 0.6
-- ("101010",33)

-- >>> runGA baseCase 10 0.6
-- ("101010",33)

-- >>> runGA baseCase 15 0.6
-- ("011101",39)

-- >>> runGA baseCase 20 0.6
-- ("011101",39)

-- >>> runGA baseCase 25 0.6
-- ("111010",40)

-- >>> runGA baseCase 30 0.6
-- ("111010",40)

-- >>> runGA baseCase 50 0.6
-- ("111010",40)


----- convert Chromosome to (Chromosome, Fitness)

buildPopulation :: Population -> Int -> [(Int, Int)] -> [ChromosomeF]
buildPopulation base limit inputData = countPoint (validCheck base limit inputData) inputData

buildPopulation' :: Population -> Int -> [(Int, Int)] -> [ChromosomeF]
buildPopulation' base limit inputData = countPoint (fitnessCal base limit inputData) inputData

fitnessCal :: [String] -> Int -> [(Int, Int)] -> [ChromosomeF]
fitnessCal [] _ _ = []
fitnessCal (x:xs) limit ys | wSum > limit || pSum == 0 = (x, -1) : fitnessCal xs limit ys
                        | otherwise = (x, pSum) : fitnessCal xs limit ys
                        where (wSum, pSum) = count x ys 0 0
-- >>> buildPopulation baseCase 30 inputData
-- [("100110",28),("001110",23),("010100",12),("011001",34)]

validCheck :: [String] -> Int -> [(Int, Int)] -> [ChromosomeF]
validCheck [] _ _ = []
validCheck (x:xs) limit ys | wSum > limit || pSum == 0 = validCheck xs limit ys
                           | otherwise = (x, 0) : validCheck xs limit ys
                           where (wSum, pSum) = count x ys 0 0

genbin :: Int -> [String]
genbin 0 = []
genbin 1 = ["0", "1"]
genbin i = let x = genbin $ i - 1
           in map ('0':) x ++ map ('1':) x

countPoint :: [ChromosomeF] -> [(Int, Int)] -> [ChromosomeF]
countPoint [] _ = []
countPoint ((x, y):xs) ys = (x, pSum) : countPoint xs ys
                       where (wSum, pSum) = count x ys 0 0

-- >>> countPoint [("111111", 0), ("111110", 0)] inputData
-- [("111111",62),("111110",45)]

-- >>> take 1 ( sortPopulation (countPoint (buildPopulation baseCase 30 inputData) inputData))
-- [("011001",34),("100110",28),("001110",23),("010100",12)]

sortPopulation :: [ChromosomeF] -> [ChromosomeF]
sortPopulation xs = reverseList (sortBy (comparing snd) xs)

reverseList :: [ChromosomeF] -> [ChromosomeF]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]

count :: Chromosome -> [(Int, Int)] -> Int -> Int -> (Int, Int)
count [] [] x y = (x, y)
count (x:xs) ((w, p):ys) wSum pSum | x == '1' = count xs ys (wSum + w) (pSum + p)
                                      | otherwise = count xs ys wSum pSum

-- >>> count "100110" inputData 0 0
-- (29,28)

----- Select

selectParents :: Int -> Int -> Int -> Int -> [ChromosomeF] -> [(Chromosome, Chromosome)]
selectParents length seed selecNum selecPairNum chroData = let chroList = toChromoList (randSelect length seed selecNum selecPairNum) chroData
                                                           in selectParents' 0 (div length 2) chroList

selectParents' :: Int -> Int -> [Chromosome] -> [(Chromosome, Chromosome)]
selectParents' pairNum limit chroList | pairNum < limit = selectParents'' pairNum 0 chroList : selectParents' (pairNum + 1) limit chroList
                                      | otherwise = []

selectParents'' :: Int -> Int -> [Chromosome] -> (Chromosome, Chromosome)
selectParents'' _ _ [x, y] = (x, y)
selectParents'' pairNum i (x:y:xs) | i < pairNum * 2 = selectParents'' pairNum (i + 1) (y:xs)
                                   | otherwise = (x, y)


randSelect :: Int -> Int -> Int -> Int -> [[Int]]
randSelect length seed num count | count /= 0 = sort (randSelect' length seed num) : randSelect length (seed + length) num (count - 1)
                                 | otherwise = []

randSelect' :: Int -> Int -> Int -> [Int]
randSelect' length seed num | num > 0 = r : randSelect' length (seed + 1) (num - 1)
                            | otherwise = []
                            where (r, y) = randomR (0, length) (mkStdGen seed)

toChromoList :: [[Int]] -> [ChromosomeF] -> [Chromosome]
toChromoList [] _ = []
toChromoList (x:xs) ys = selectFilter x ys : toChromoList xs ys

selectFilter :: [Int] -> [ChromosomeF] -> Chromosome
selectFilter xs ys = findBiggest (toChromoComList xs ys) (-1) ("", 0)

toChromoComList :: [Int] -> [ChromosomeF] -> [ChromosomeF]
toChromoComList [] _ = []
toChromoComList (x:xs) ys = toChromoComList' x 0 ys : toChromoComList xs ys

toChromoComList' :: Int -> Int -> [ChromosomeF] -> ChromosomeF
toChromoComList' stop i [x] = x
toChromoComList' stop i (x:xs) | i < stop = toChromoComList' stop (i + 1) xs
                               | otherwise = x


findBiggest :: [ChromosomeF] -> Int -> ChromosomeF -> Chromosome
findBiggest [] i (s, f) = s
findBiggest ((x, y):xs) i (s, f) | y > f = findBiggest xs (i + 1) (x, y)
                                 | otherwise = findBiggest xs i (s, f)

----- Crossover

crossover :: (Chromosome, Chromosome) -> (Chromosome, Chromosome)
crossover ([], []) = ([], [])
crossover (xs, ys) = let (c1s1, c1s2) = splitChromo xs (div (length xs) 2)
                         (c2s1, c2s2) = splitChromo ys (div (length xs) 2)
                     in (c1s1 ++ c2s2, c2s1 ++ c1s2)

splitChromo :: Chromosome ->  Int -> (Chromosome, Chromosome)
splitChromo xs p = let c1s1 = splitChromo' xs 0 0 ((div (length xs) 2) - 1)
                       c1s2 = splitChromo' xs 0 (div (length xs) 2) ((length xs) - 1)
                   in (c1s1, c1s2)

splitChromo' :: Chromosome -> Int -> Int -> Int -> Chromosome
splitChromo' [] _ _ _ = []
splitChromo' (x:xs) i si p | i < si = splitChromo' xs (i+1) si p
                           | i >= si && i <= p = x : splitChromo' xs (i+1) si p
                           | otherwise = []

----- Mutation
mutation :: Chromosome -> Float -> Int -> Chromosome
mutation [] _ _ = []
mutation (x:xs) percent seed | flag = if x == '0'
                                        then '1' : mutation xs percent (seed + 10)
                                      else '0' : mutation xs percent (seed + 10)
                             | otherwise = x : mutation xs percent (seed + 10)
                             where flag = percentRan percent seed

percentRan :: Float -> Int -> Bool
percentRan x seed | x * 10 > r = True
                  | otherwise = False
                  where  (r, y) = randomR (1, 10) (mkStdGen seed)

