import Data.List

class (Eq c, Show c) => Config c where
  successors :: c -> [c]
  isGoal :: c -> Bool


solveAll :: (Config c) =>  c -> [c]
solveAll c = let restSolutions = concat [solveAll c' | c' <- successors c] 
                    in if isGoal c then c:restSolutions else restSolutions

solve :: (Config c) => c -> (Maybe c)
solve c = case solveAll  c of
              []   -> Nothing
              x:xs -> Just x


--1
--this data type is either boolean/string/(and/or/not operation on datatype of its own kind) 
data BExp = BConst Bool|Var String|And BExp BExp|Or BExp BExp| Not BExp deriving Eq


constructor :: BExp -> [(String, Bool)] -> Bool
constructor (And a b) table = (constructor a table) && (constructor b table)
constructor (Or a b) table = (constructor a table) || (constructor b table)
constructor (Not a) table = not (constructor a table)
constructor (BConst a) table = a
constructor (Var a) table = findValue table a

--2
data SatConfig = SatConfig [(String, Bool)] [String] BExp deriving Eq

instance Show SatConfig where
	show(SatConfig tpls vars exp) = show(tpls)




instance Config SatConfig where
  successors (SatConfig tpls [] _) = []
  successors (SatConfig tpls (var:vars) exp) = [(SatConfig ((var, b):tpls) vars exp)| b <- [True,False]]
  --successors (SatConfig tpls (var:vars) exp) = successors' exp (var:vars) (findAllCombinations (var:vars))
  isGoal (SatConfig tpls [] exp) = constructor exp tpls
  isGoal (SatConfig tpls vars exp) = False


--3

satSolve :: BExp -> Maybe SatConfig
satSolve vars =
	let sigma = findString vars
	in solve (SatConfig [] sigma vars)

--satSolve (And (Var "a") (Var "b"))
--satSolve (Not (Var "a"))
--satSolve (Or (Not (Var "a")) (And (Not (Var "b")) (Var "c")))
--satSolve (And (Not (Var "a")) (And (Not (Var "b")) (Var "c")))
--satSolve (Not (Or (Not (Var "a")) (And (Not (Var "b")) (Var "c"))))


----------------------------------------------helper for constructor---------------------------------------------------------------------
--this function is similar to one in CryptConfig Example 
--the purpose of the function is to return the value of the variable  string from from a tupple (String,Bool) e.g ("a", True) returns True
findValue :: [(String,Bool )] -> String -> Bool                        
findValue env c = case lookup c env of
                Nothing -> error ("undefined variable: " ++ (show c))
                Just nBoolean  -> nBoolean

----------------------------------------------helper for satSolve---------------------------------------------------------------------
--this function will return the string in the expression
findString :: BExp -> [String]
findString (BConst a) = []
findString (And a b) = nub((findString a) ++ (findString b))
findString (Var a) = [a]
findString (Or a b) = (findString a) ++ (findString b)
findString (Not a) = (findString a) 

--findString (Or (Var "a") (Var "b"))
--findString (Or (Not (Var "a")) (And (Not (Var "b")) (Var "c")))
-------------------------------------------------------------------------------------------------------------------


main = do
  print(solveAll (SatConfig [] ["x", "y"] (And (Var "x") (Var "y"))))
