-------- 1
data Const = IConst  Integer |
             BConst  Bool    |
             FConst1 String (Const -> Const)   |
             FConst2 String (Const -> Const -> Const)

instance Show Const where
  show (IConst x)      = show x
  show (BConst x)      = show x
  show (FConst1 str f) = "<prim1:" ++ str ++ ">"
  show (FConst2 str f) = "<prim2:" ++ str ++ ">"

instance Eq Const where
  IConst x == IConst y                 = x == y
  BConst x == BConst y                 = x == y
  (FConst1 str1 x) == (FConst1 str2 y) = str1 == str2
  (FConst2 str1 x) == (FConst2 str2 y) = str1 == str2

primAbs :: Const -> Const
primAbs (IConst x) | x < 0     = IConst (-x)
                   | otherwise = IConst x

primPlus :: Const -> Const -> Const
primPlus (IConst x) (IConst y) = IConst (x + y)

primEq :: Const -> Const -> Const
primEq (IConst x) (IConst y) | x == y = BConst True
                             | otherwise = BConst False
primEq _ _ = BConst False     


-- >>> IConst 5
-- 5
-- >>> BConst True
-- True

-- >>> FConst "abs" primAbs
-- <prim1:abs>

-- >>> FConst2 "+" primPlus
-- <prim2:+>

-- >>> FConst2 "==" primEq
-- <prim2:==>

-------- 2
data Exp = Econ Const        |
           Var String        |
           Lambda String Exp |
           IfExp Exp Exp Exp |
           Appl Exp Exp      |
           LetRec Exp deriving (Eq, Show)

-- >>> Econ (IConst 2)
-- Econ 2

-- >>> Var "x"
-- Var "x"

-- >>> Lambda "x" (Var "x")
-- Lambda "x" (Var "x")

-- >>> IfExp (Econ (BConst True)) (Var "x") (Var "y")
-- IfExp (Econ True) (Var "x") (Var "y")

-- >>> Appl (Lambda "x" (Var "x")) (Econ (IConst 2))
-- Appl (Lambda "x" (Var "x")) (Econ 2)

-------- 3
data Operator = S | K | I | B | C | CIf | Y deriving (Eq, Show)

data CExp = Ccon Const |
            CVar String |
            Cop Operator |
            CAppl CExp CExp deriving (Eq, Show)

-- >>> Ccon (IConst 5)
-- Ccon 5

-- >>> CVar "x"
-- CVar "x"

-- >>> Cop S
-- Cop S

-- >>> Cop CIf
-- Cop CIf

-- >>> CAppl (Cop I) (CVar "x")
-- CAppl (Cop I) (CVar "x")

-- >>> CAppl (CAppl (Cop S) (Cop K)) (Cop K)
-- CAppl (CAppl (Cop S) (Cop K)) (Cop K)

-------- 4
---- a
compile :: Exp -> CExp
compile (Econ c)         = Ccon c
compile (Var x)          = CVar x
compile (IfExp e1 e2 e3) = CAppl (CAppl (CAppl (Cop CIf) (compile e1)) (compile e2)) (compile e3)
compile (Lambda str e)   = abstract str (compile e)
compile (Appl e1 e2)     = CAppl (compile e1) (compile e2)
compile (LetRec e)       = CAppl (Cop Y) (compile e)

abstract :: String -> CExp -> CExp
abstract var (Ccon c)    = CAppl (Cop K) (Ccon c)
abstract var (CVar x)    | var == x = Cop I
                         | otherwise = CAppl (Cop K) (CVar x)
abstract var (Cop x)     = CAppl (Cop K) (Cop x)
abstract var (CAppl m n) = let m' = abstract var m
                               n' = abstract var n
                          in case m' of 
                            CAppl (Cop K) x -> case n' of Cop I           -> x 
                                                          CAppl (Cop K) y -> CAppl (Cop K) (CAppl x y)
                                                          _               -> CAppl (CAppl (Cop B) x) (simplify n')
                            _ -> case n' of CAppl (Cop K) y -> CAppl (CAppl (Cop C) (simplify m')) y
                                            _               -> CAppl (CAppl (Cop S) (simplify m')) (simplify n')

simplify :: CExp -> CExp
simplify (CAppl (Cop K) x) = x
simplify x = x

---- b
replaceGlobal :: [(String, Const -> Const)] -> [(String, Const -> Const -> Const)] -> CExp -> CExp
replaceGlobal [] [] (CVar m) = CVar m
replaceGlobal ((s1, f1) : initEnv1) [] (CVar m) | m == s1 = Ccon (FConst1 m f1)
                                                | otherwise = replaceGlobal initEnv1 [] (CVar m)
replaceGlobal [] ((s2, f2) : initEnv2) (CVar m) | m == s2 = Ccon (FConst2 m f2)
                                                | otherwise = replaceGlobal [] initEnv2 (CVar m)
replaceGlobal ((s1, f1) : initEnv1) ((s2, f2) : initEnv2) (CVar m) | m == s1 = Ccon (FConst1 m f1)
                                                                   | m == s2 = Ccon (FConst2 m f2)
                                                                   | otherwise = replaceGlobal initEnv1 initEnv2 (CVar m)
replaceGlobal initEnv1 initEnv2 (CAppl ce1 ce2) = CAppl (replaceGlobal initEnv1 initEnv2 ce1) (replaceGlobal initEnv1 initEnv2 ce2)
replaceGlobal _ _ x = x

compileReplacing :: Exp -> CExp
compileReplacing x = replaceGlobal [("abs",primAbs)] [("+", primPlus), ("==", primEq)] (compile x)

-- >>> replaceGlobal [("abs",primAbs)] [("+",primPlus)]  (CVar "abs")
-- Ccon <prim1:abs>

-- >>> compileReplacing (Econ (IConst 5))
-- Ccon 5

-- >>> compileReplacing (Var "x")
-- CVar "x"

-- >>> compileReplacing (IfExp (Var "x") (Var "y") (Var "z"))
-- CAppl (CAppl (CAppl (Cop CIf) (CVar "x")) (CVar "y")) (CVar "z")

-- >>> compileReplacing  (Lambda "x" (Var "x"))
-- Cop I

-- >>> compileReplacing (Var "abs")
-- Ccon <prim1:abs>

-- >>> compileReplacing  (Appl (Var "abs") (Econ (IConst (-2))))
-- CAppl (Ccon <prim1:abs>) (Ccon -2)

-- >>> compileReplacing (Lambda "x" (Appl (Var "abs") (Var "x")))
-- Ccon <prim1:abs>

-- >>> compileReplacing (Lambda "x" (Appl (Appl (Var "+") (Var "x")) (Econ (IConst 1))))
-- CAppl (CAppl (Cop C) (Ccon <prim2:+>)) (Ccon 1)


-------- 5 
reduceComb :: CExp -> CExp
reduceComb (CAppl (Cop I) x)                                                          = x
reduceComb (CAppl (CAppl (Cop K) x) y)                                                = x
reduceComb (CAppl (CAppl (CAppl (Cop S) f) g) x)                                      = CAppl (CAppl f x) (CAppl g x)
reduceComb (CAppl (CAppl (CAppl (Cop B) f) g) x)                                      = CAppl f (CAppl g x)
reduceComb (CAppl (CAppl (CAppl (Cop C) f) x) y)                                      = CAppl (CAppl f y) x
reduceComb (CAppl (CAppl (CAppl (Cop CIf) (Ccon (BConst True))) x) y)                 = x
reduceComb (CAppl (CAppl (CAppl (Cop CIf) (Ccon (BConst False))) x) y)                = y
reduceComb (CAppl (Ccon (FConst1 str f)) (Ccon (IConst x)))                           = Ccon (f (IConst x))
reduceComb (CAppl (CAppl (Ccon (FConst2 str f)) (Ccon (IConst x))) (Ccon (IConst y))) = Ccon (f (IConst x) (IConst y))
reduceComb (CAppl (CAppl (Cop Y) (Ccon (FConst2 str f))) x)                           = CAppl (CAppl (Cop Y) (Ccon (FConst2 str f))) (CAppl (Ccon (FConst2 str f)) x)
reduceComb (CAppl x y)                                                                = CAppl (reduceComb x) (reduceComb y)
reduceComb x                                                                          = x

-- >>> reduceComb (CAppl (Cop I) (CVar "X"))
-- CVar "X"

-- >>> reduceComb (CAppl (CAppl (Cop K) (CVar "X")) (CVar "Y"))
-- CVar "X"

-- >>> reduceComb (CAppl (CAppl (CAppl (Cop S) (CVar "F")) (CVar "G")) (CVar "X"))
-- CAppl (CAppl (CVar "F") (CVar "X")) (CAppl (CVar "G") (CVar "X"))

-- >>> reduceComb (CAppl (CAppl (CAppl (Cop B) (CVar "F")) (CVar "G")) (CVar "X"))
-- CAppl (CVar "F") (CAppl (CVar "G") (CVar "X"))

-- >>> reduceComb (CAppl (CAppl (CAppl (Cop C) (CVar "F")) (CVar "X")) (CVar "Y"))
-- CAppl (CAppl (CVar "F") (CVar "Y")) (CVar "X")

-- >>>  reduceComb (CAppl (CAppl (CAppl (Cop CIf) (Ccon (BConst True))) (CVar "X")) (CVar "Y"))
-- CVar "X"

-- >>> reduceComb (CAppl (CAppl (CAppl (Cop CIf) (Ccon (BConst False)))(CVar "X"))(CVar "Y"))
-- CVar "Y"

-- >>>  reduceComb (CAppl (Ccon (FConst1 "abs" primAbs)) (Ccon (IConst (-2))))
-- Ccon 2

-- >>>  reduceComb (CAppl (CAppl (Ccon (FConst2 "+" primPlus))(Ccon (IConst (-2))))(Ccon (IConst 5)))
-- Ccon 3

-- >>> reduceComb (CAppl (Ccon (FConst1 "abs" primAbs)) (Ccon (IConst (-2))))
-- Ccon 2

-- >>>reduceComb (CAppl (CAppl (Ccon (FConst2 "+" primPlus))(Ccon (IConst (-2))))(Ccon (IConst 5)))
-- Ccon 3


-------- 6
run :: CExp -> CExp
run (CVar x) = CVar x
run (Ccon x) = Ccon x
run x        | x /= reduceComb x =  run (reduceComb x)
             | otherwise = x

-- >>> run  (CAppl (Cop I) (CAppl (Cop I) (Ccon (IConst 5))))
-- Ccon 5

-------- 7
compileAndRun :: Exp -> CExp
compileAndRun e = run (compileReplacing e)

-- >>> compileAndRun (Var "abs")
-- Ccon <prim1:abs>

-- >>> compileAndRun (Appl (Var "abs") (Econ (IConst (-2))))
-- Ccon 2

-- >>> compileAndRun (Appl (Lambda "x" (Var "x")) (Econ (IConst 5)))
-- Ccon 5

-- >>> compileAndRun (Appl (Lambda "x" (Econ (IConst 5))) (Appl (Appl (Var "div") (Econ (IConst 1))) (Econ (IConst 0))))
-- Ccon 5

-- >>>compileAndRun (Appl (Lambda "x" (IfExp (Appl (Appl (Var"==") (Var "x"))(Econ (IConst 0)))(Var "x")(Appl (Appl (Var "+")(Var "x"))(Econ (IConst 1)))))(Econ (IConst 3)))
-- Ccon 4

-- >>>  compileAndRun (Appl (Lambda "x" (IfExp (Appl (Appl (Var"==") (Var "x"))(Econ (IConst 0)))(Var "x")(Appl (Appl (Var "+")(Var "x"))(Econ (IConst 1)))))(Econ (IConst 0)))
-- Ccon 0

-------- Extra 1
-- >>> LetRec (Econ (IConst 5))
-- LetRec (Econ 5)

-- >>> compile (Appl (LetRec (Econ (FConst2 "+" primPlus))) (Econ (IConst 5)))
-- CAppl (CAppl (Cop Y) (Ccon <prim2:+>)) (Ccon 5)

-- >>> reduceComb (CAppl (CAppl (Cop Y) (Ccon (FConst2 "+" primPlus))) (Ccon (IConst 5)))
-- CAppl (CAppl (Cop Y) (Ccon <prim2:+>)) (CAppl (Ccon <prim2:+>) (Ccon 5))

-- >>> reduceComb (reduceComb (CAppl (CAppl (Cop Y) (Ccon (FConst2 "+" primPlus))) (Ccon (IConst 5))))
-- CAppl (CAppl (Cop Y) (Ccon <prim2:+>)) (CAppl (Ccon <prim2:+>) (CAppl (Ccon <prim2:+>) (Ccon 5)))
