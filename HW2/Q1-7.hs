--generic solver
class (Eq c, Show c) => Config c where
  successors :: c -> [c]
  isGoal :: c -> Bool

solveAll :: (Config c) => c -> [c]
solveAll c = let restSolutions = concat [solveAll c' | c' <- successors c]
             in if isGoal c then c:restSolutions else restSolutions

solve :: (Config c) => c -> (Maybe c)
solve c = case solveAll c of
            []   -> Nothing
            x:xs -> Just x

--1
data SudokuConfig = SudokuConfig [[Integer]]

--2
sudokuConfigFromList :: [Integer] -> SudokuConfig
sudokuConfigFromList [] = SudokuConfig []
sudokuConfigFromList xs = SudokuConfig (to2DList xs 0 9 81)

to2DList :: [Integer] -> Integer -> Integer -> Integer -> [[Integer]]
to2DList [] i j u = [[]]
to2DList xs i j u | i < u = to2DList' xs i j : to2DList xs (i+j) j u
                  | otherwise = []

to2DList' :: [Integer] -> Integer -> Integer -> [Integer]
to2DList' [] _ _ = []
to2DList' (x:xs) i j | i /= 0 = to2DList' xs (i-1) j
                     | otherwise = to2DList'' (x:xs) 0 j

to2DList'' :: [Integer] -> Integer -> Integer -> [Integer]
to2DList'' [] _ _ = []
to2DList'' (x:xs) i j | i /= j = x : to2DList'' xs (i+1) j
                      | otherwise = []

--3
listFromSudokuConfig :: SudokuConfig -> [Integer]
listFromSudokuConfig (SudokuConfig [[]]) = []
listFromSudokuConfig (SudokuConfig xs) = toList xs

toList :: [[Integer]] -> [Integer]
toList = concat

--4
instance Eq SudokuConfig where
  (==) (SudokuConfig e1) (SudokuConfig e2) = e1 == e2

--5
instance Show SudokuConfig where
  show (SudokuConfig []) = []
  show (SudokuConfig xs) = customPrint xs 0

customPrint :: [[Integer]] -> Integer -> String
customPrint [] _ = ""
customPrint (x:xs) i | rem i 27 /= 18 && i /= 0 = customPrint' x i ++ "\n" ++ customPrint xs (i+9)
                     | rem i 27 /= 18 && i == 0  = "\n" ++ customPrint' x i ++ "\n" ++ customPrint xs (i+9)
                     | otherwise  = customPrint' x i ++ "\n\n" ++ customPrint xs (i+9)

customPrint' :: [Integer] -> Integer -> String
customPrint' [] _ = ""
customPrint' (x:xs) i | (x == 0) && (rem i 3 /= 2) = "_ " ++ customPrint' xs (i+1)
                      | (x == 0) && (rem i 3 == 2) = "_  " ++ customPrint' xs (i+1)
                      | (x /= 0) && (rem i 3 /= 2) = show x ++ " " ++ customPrint' xs (i+1)
                      | otherwise = show x ++ "  " ++ customPrint' xs (i+1)

--6
instance Config SudokuConfig where
  successors s = generateSuccessors (getPossibleValues s) s
  isGoal s | isRowDuplicate s || isColumDuplicate s || isGridDuplicate s = False
           | otherwise = True

--Successor Helper
findNextEmptySlot :: SudokuConfig -> (Integer, Integer)
findNextEmptySlot (SudokuConfig xs) = findNextEmptySlot' (toList xs) 0

findNextEmptySlot' :: [Integer] -> Integer -> (Integer, Integer)
findNextEmptySlot' (x:xs) i | x == 0 = (rem i 9, div i 9)
                            | otherwise = findNextEmptySlot' xs (i + 1)

addSuccessor :: SudokuConfig -> [SudokuConfig]
addSuccessor s = generateSuccessors (getPossibleValues s) s

generateSuccessors :: [Integer] -> SudokuConfig -> [SudokuConfig]
generateSuccessors xs s = map (\ x -> setNewValue x (findNextEmptySlot s) s) xs

setNewValue :: Integer -> (Integer,Integer) -> SudokuConfig -> SudokuConfig
setNewValue v (x,y) (SudokuConfig ts) | (x > 8) || (y > 8) || (x < 0) || (y < 0) = SudokuConfig ts
                                    | otherwise = sudokuConfigFromList (setNewValue' v (x + y * 9) (toList ts))

setNewValue' :: Integer -> Integer -> [Integer] -> [Integer]
setNewValue' v i (x:xs) | i /= 0 = x:setNewValue' v (i-1) xs
                        | (i == 0) && x /= 0 = x:xs
                        | otherwise = v:xs

--Calculate possibility
getPossibleValues :: SudokuConfig -> [Integer]
getPossibleValues (SudokuConfig xs) = getPossibility (SudokuConfig xs) (findNextEmptySlot (SudokuConfig xs))

getPossibility :: SudokuConfig -> (Integer, Integer) -> [Integer]
getPossibility (SudokuConfig xs) (x,y) = getExistNums (concat (getRow (SudokuConfig xs) (x,y):
                                                               getColumn (SudokuConfig xs) (x,y):
                                                               getGrid (SudokuConfig xs) (x,y):[[]]))

getExistNums :: [Integer] -> [Integer]
getExistNums xs = getExistNums' (deleteDuplicate (deleteZero (quicksort xs))) [1..9]

deleteDuplicate :: [Integer] -> [Integer]
deleteDuplicate [x] = [x]
deleteDuplicate (x:y:xs) | x == y = deleteDuplicate (x:xs)
                         | otherwise = x:deleteDuplicate (y:xs)

deleteZero :: [Integer] -> [Integer]
deleteZero (x:xs) | x == 0 = xs
                  | otherwise = x:xs

getExistNums' :: [Integer] -> [Integer] -> [Integer]
getExistNums' [] [y] = [y]
getExistNums' _ [] = []
getExistNums' (x:xs) (y:ys) | x < y = getExistNums' xs (y:ys)
                            | x == y = getExistNums' xs ys
                            | otherwise = y : getExistNums' (x:xs) ys

-- Get three types of lists
getRow :: SudokuConfig -> (Integer, Integer) -> [Integer]
getRow (SudokuConfig (t:ts)) (x,y) =
 let getRow' (v:vs) i | y /= i = getRow' vs (i+1)
                      | otherwise = v
 in getRow' (t:ts) 0

getColumn :: SudokuConfig -> (Integer, Integer) -> [Integer]
getColumn (SudokuConfig ts) (x,y) = getColumn' (toColumnList (SudokuConfig ts)) (x,y) 0

getColumn' :: [[Integer]] -> (Integer, Integer) -> Integer -> [Integer]
getColumn' (t:ts) (x,y) i | x /= i = getColumn' ts (x,y) (i+1)
                          | otherwise = t

getGrid :: SudokuConfig -> (Integer, Integer) -> [Integer]
getGrid  (SudokuConfig ts) (x,y) = getGrid' (toGridList (SudokuConfig ts)) ((div x 3) + (div y 3) * 3) 0

getGrid' :: [[Integer]] -> Integer -> Integer -> [Integer]
getGrid' (t:ts) n i | n /= i = getGrid' ts n (i+1)
                    | otherwise = t


--isGoal Helper
-- Row
isRowDuplicate :: SudokuConfig -> Bool
isRowDuplicate (SudokuConfig []) = False
isRowDuplicate (SudokuConfig (x:xs)) | isListNumDuplicate x [1..9] = True
                                     | otherwise = isRowDuplicate (SudokuConfig xs)

isListNumDuplicate :: [Integer] -> [Integer] -> Bool
isListNumDuplicate [] _ = False
isListNumDuplicate xs ts = quicksort xs /= ts

quicksort :: Ord a => [a] -> [a]
quicksort []     = []
quicksort (p:xs) = quicksort [x | x <- xs, x < p] ++
                   [x | x <- p:xs, x == p]        ++
                   quicksort [x | x <- xs , x > p]
-- Column
isColumDuplicate :: SudokuConfig -> Bool
isColumDuplicate (SudokuConfig xs) = isColumDuplicate' (toColumnList (SudokuConfig xs))

isColumDuplicate' :: [[Integer]] -> Bool
isColumDuplicate' [] = False
isColumDuplicate' (x:xs) | isListNumDuplicate x [1..9] = True
                         | otherwise = isColumDuplicate' xs

toColumnList :: SudokuConfig -> [[Integer]]
toColumnList (SudokuConfig (x:xs)) = toColumnList' (x:xs) 0 9

toColumnList' :: [[Integer]] -> Integer -> Integer ->  [[Integer]]
toColumnList' xs columnNum i | columnNum /= i = getColumnList xs columnNum : toColumnList' xs (columnNum+1) i
                             | otherwise = []

getColumnList :: [[Integer]] -> Integer -> [Integer]
getColumnList [] _ = []
getColumnList (x:xs) i =
  let getColumnNum (x:xs) i | i == 0 = x
                            | otherwise = getColumnNum xs (i-1)
  in getColumnNum x i : getColumnList xs i

-- Grid
isGridDuplicate :: SudokuConfig -> Bool
isGridDuplicate (SudokuConfig xs) = isGridDuplicate' xs

isGridDuplicate' :: [[Integer]] -> Bool
isGridDuplicate' [] = False
isGridDuplicate' (x:xs) | isListNumDuplicate x [1..9] = True
                        | otherwise = isGridDuplicate' xs

toGridList :: SudokuConfig -> [[Integer]]
toGridList (SudokuConfig xs) = toGridList' (toList xs) 0

toGridList' :: [Integer] -> Integer -> [[Integer]]
toGridList' xs i | i < 9 = toGridList'' xs i : toGridList' xs (i+1)
              | otherwise = []

toGridList'' :: [Integer] -> Integer -> [Integer]
toGridList'' xs i = getNums xs xs (calPosi i)

getNums :: [Integer] -> [Integer] -> [Integer] -> [Integer]
getNums _ _ [] = []
getNums (x:xs) ts (y:ys) | y == 0 = x : getNums ts ts ys
                         | otherwise = getNums xs ts ((y-1):ys)

calPosi :: Integer -> [Integer]
calPosi = calOffest (calBasePosi 0 0)

calOffest :: [Integer] -> Integer -> [Integer]
calOffest [] _ = []
calOffest (x:xs) i = x + (div i 3 * 27 + rem i 3 * 3) : calOffest xs i

calBasePosi :: Integer -> Integer-> [Integer]
calBasePosi i j | rem i 3 < 2 && j < 3  = i : calBasePosi (i+1) j
            | rem i 3 == 2 && j < 3 = i : calBasePosi (9*(j+1)) (j+1)
            | otherwise = []


--7
sudokuSolve :: SudokuConfig -> Maybe SudokuConfig
sudokuSolve = solve





