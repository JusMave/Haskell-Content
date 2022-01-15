--Extra
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
data BExp = BConst Bool
          | Var String
          | And BExp BExp
          | Or BExp BExp
          | Not BExp
          deriving (Show, Eq)

--2

data SatConfig = SatConfig BExp [String] [(String, Bool)] deriving Eq

instance Show SatConfig where
    show (SatConfig _ _ varValues) = show varValues

instance Config SatConfig where
    successors (SatConfig _ [] _) = []
    successors (SatConfig exp varNames varValues) = addSuccessor (SatConfig exp varNames varValues)
    isGoal (SatConfig exp [] varValues) = getResult exp varValues
    isGoal (SatConfig exp varNames varValues) = False
    
--successors Helper
addSuccessor :: SatConfig -> [SatConfig]
addSuccessor (SatConfig exp (x:xs) varValues) = [SatConfig exp xs ((x,True):varValues), SatConfig exp xs ((x,False):varValues)]

--isGoal Helper
getResult :: BExp -> [(String, Bool)] -> Bool
getResult (BConst v) _           = v
getResult (Var v) values         = getValue values v
getResult (Not exp) values       = not (getResult exp values)
getResult (And exp1 exp2) values = getResult exp1 values && getResult exp2 values
getResult (Or exp1 exp2) values  = getResult exp1 values || getResult exp2 values

getValue :: [(String, Bool)] -> String -> Bool
getValue ((x,y):xs) name | name == x = y
                         | otherwise = getValue xs name

--3
satSolve :: BExp -> Maybe SatConfig
satSolve exp = solve (SatConfig exp (getVarNames exp []) [])

getVarNames :: BExp -> [String] -> [String]
getVarNames (And exp1 exp2) xs = getVarNames exp1 xs ++ getVarNames exp2 xs
getVarNames (Or exp1 exp2) xs  = getVarNames exp1 xs ++ getVarNames exp2 xs
getVarNames (Not exp) xs       = getVarNames exp xs
getVarNames (Var name) xs      = name:xs
getVarNames (BConst value) xs  = []

--satSolve (And (Var "a") (Var "b"))
--satSolve (And (Var "a") (BConst True))
--satSolve (Not (Var "a"))
--satSolve (Or (Var "a") (Var "b"))
--satSolve (And (And (Var "a") (Var "b")) (Var "c"))

