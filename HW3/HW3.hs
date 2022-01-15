{-# LANGUAGE DatatypeContexts #-}
import Series
import Sequences
import MPoly
import Data.Ratio
import Data.List


--------- 1

sinPowSeD :: PowSe Double
sinPowSeD = integratePlus cosPowSeD 0


sinPowSeR :: PowSe Rational
sinPowSeR = integratePlus cosPowSeR 0

cosPowSeD :: PowSe Double
cosPowSeD = integratePlus (-sinPowSeD) 1

cosPowSeR :: PowSe Rational
cosPowSeR = integratePlus (-sinPowSeR) 1


-- 1 TEST
-- sinPowSeD
-- 1.0*x-0.16666666666666666*x^3+8.333333333333333e-3*x^5-1.984126984126984e-4*x^7+2.7557319223985893e-6*x^9...

-- sinPowSeR
-- 1 % 1*x-1 % 6*x^3+1 % 120*x^5-1 % 5040*x^7+1 % 362880*x^9...

-- cosPowSeD
-- 1.0-0.5*x^2+4.1666666666666664e-2*x^4-1.388888888888889e-3*x^6+2.48015873015873e-5*x^8-...

-- cosPowSeR
-- 1 % 1-1 % 2*x^2+1 % 24*x^4-1 % 720*x^6+1 % 40320*x^8-...

--------- 2

mySine :: (Fractional a, Ord a) => a -> a
mySine angle | angle < 0.000000001 && angle >= 0 = angle
             | angle > -0.000000001 && angle <= 0 = angle
             | otherwise = let x = mySine (angle / 3)
                           in 3 * x - 4 * x ^ 3

-- 2 TEST
-- mySine 90
-- 0.8939966636005

-- mySine 45
-- 0.8509035245341463

-- mySine 0
-- 0.0

--------- 3
------ a
sub :: Num a => [a] -> [a] -> [a]
sub [] [] = []
sub (x:xs) (y:ys) = (x - y) : sub xs ys

-- sub [2,2,3] [2,5,12] 
-- [0,-3,-9]

------ b

scaleList :: Num a => a -> [a] -> [a]
scaleList _ [] = []
scaleList t (x:xs) = (t * x) : scaleList t xs

-- scaleList (1/2) [2,5,12]
-- [1.0,2.5,6.0]

------ c
subScale :: (Fractional a) => [a] -> [a] -> [a]
subScale (x:xs) (y:ys) = drop 1 (sub (y:ys) (scaleList (y / x) (x:xs)))

-- subScale [2,2,3,10] [2,5,12,31]
-- [3.0,9.0,21.0]

-- subScale [0,1,2] [1,2,3]
-- [-Infinity,-Infinity]

------ d
nonZeroFirst :: (Eq a, Num a) => [[a]] -> [[a]]
nonZeroFirst xs | isValid xs 0 = error "No non Zero list"
                | otherwise = findNonZeroList xs [] []

isValid :: (Eq a, Num a) => [[a]] -> Int -> Bool
isValid [] t | t >= 1 = False
             | otherwise = True
isValid (x:xs) t | checkFirstIndex x 0 = isValid xs t
                 | otherwise = isValid xs (t + 1)

findNonZeroList :: (Eq a, Num a) => [[a]] -> [[a]] -> [[a]] -> [[a]]
findNonZeroList [] ys zs = ys ++ zs
findNonZeroList (x:xs) ys zs | checkFirstIndex x 0 = findNonZeroList xs ys (x:zs)
                             | otherwise = findNonZeroList xs (x:ys) zs

checkFirstIndex :: (Eq a, Num a) => [a] -> a -> Bool
checkFirstIndex (x:xs) t | x == t = True
                         | otherwise = False

-- nonZeroFirst  [[0,-5,-5],[-8,-4,-12]]
-- [[-8,-4,-12],[0,-5,-5]]

-- nonZeroFirst  [[0,-5,-5],[0,-5,-5]]
-- No non Zero list

------ e

triangulate :: (Eq a, Num a, Fractional a) => [[a]] -> [[a]]
triangulate [] = []
triangulate (x:xs) | checkFirstIndex x 0 = triangulate (nonZeroFirst (x:xs))
                   | otherwise = x:triangulate(subAll x xs)


subAll :: (Fractional a, Eq a) => [a] -> [[a]] -> [[a]]
subAll _ [] = []
subAll x (y:ys) = subScale x y : subAll x ys

-- triangulate [[2,3,3,8],[2,3,-2,3],[4,-2,2,4]]
-- [[2.0,3.0,3.0,8.0],[-8.0,-4.0,-12.0],[-5.0,-5.0]]

------ f
dot :: Num a => [a] -> [a] -> a
dot [] [] = 0
dot xs [] = 0
dot [] ys = 0
dot (x:xs) (y:ys) = x * y + dot xs ys

-- dot [1,2] [3,4]
-- 11

------ g

solveLine :: Fractional a => [a] -> [a] -> a
solveLine (x:xs) ys = (lastElement (x:xs) - dot xs ys) / x

lastElement :: [a] -> a
lastElement [x] = x
lastElement (x:xs) = lastElement xs

-- solveLine [2,3,3,8] [1,1]
-- 1.0


-- solveLine [2,3,3,8] [0]
-- 4.0


------ h
solveTriangular :: Fractional a => [[a]] -> [a]
solveTriangular [x] = [solveLine x [0]]
solveTriangular (x:xs) = solveLine x (solveTriangular xs) : solveTriangular xs

-- solveTriangular [[2.0,3.0,3.0,8.0],[-8.0,-4.0,-12.0],[-5.0,-5.0]]
-- [1.0,1.0,1.0]

------ i

solveSystem :: (Fractional a, Eq a) => [[a]] -> [a]
solveSystem xs = solveTriangular (triangulate xs)

-- solveSystem [[2,3,3,8],[2,3,-2,3],[4,-2,2,4]]
-- [1.0,1.0,1.0]

--------- 4

data Exp = RExp Rational  |
           Var String     |
           Sum Exp Exp    |
           Prod Exp Exp   |
           D Exp String   |
           Ap Op Exp      |
           Int Exp String |
           Pow Exp Exp  deriving Eq

instance Show Exp where
  show (RExp n)
    | n > 0 && denominator n == 1           = show (numerator n)
    | n > 0 && denominator n /= 1           = show (numerator n) ++ "/" ++ show (denominator n)
    | n < 0 && denominator n == 1           = "(" ++ show (numerator n) ++ ")"
    | n < 0 && denominator n /= 1           = "(" ++ show (numerator n) ++ "/" ++ show (denominator n) ++ ")"
    | otherwise                             = show 0
  show (Var u)                      = u
  show (Sum u v)                    = showSumContext (Sum u v)
  show (Prod (Sum x y) v)           = showProdContext (Prod (Sum x y) v)
  show (Prod u (Sum x y))           = showProdContext (Prod u (Sum x y))
  show (Prod u v)                   = showProdContext (Prod u v)
  show (Pow (Pow x y) v)            = showPowContextLeft (Pow x y) ++ show v
  show (Pow u (Pow x y))            = showPowContextLeft u ++ show (Pow x y)
  show (Pow u v)                    = showPowContextLeft u ++ showPowContextRight v
  show (D u v)                      = "D(" ++ show u ++ "," ++ v ++ ")"
  show (Ap Sin u)                   = "Sin(" ++ show u ++ ")"
  show (Ap Cos u)                   = "Cos(" ++ show u ++ ")"
  show (Ap Exp u)                   = "e^(" ++ show u ++ ")"
  show (Ap Ln u)                    = "log(" ++ show u ++ ")"
  show (Int u v)                      = "Int(" ++ show u ++ ")"


addParens :: String -> String
addParens a = "(" ++ a ++ ")"


showSumContext :: Exp -> String
showSumContext (Sum u v) = show u ++ "+" ++ show v

showProdContext :: Exp -> String
showProdContext (Prod (Sum x y) v) = "(" ++ show (Sum x y) ++ ")" ++ "*" ++ show v
showProdContext (Prod u (Sum x y)) = show u ++ "*" ++ "(" ++ show (Sum x y) ++ ")"
showProdContext (Prod u v) = show u ++ "*" ++ show v


showPowContextLeft :: Exp -> String
showPowContextLeft (RExp u) | denominator u /= 1 = "(" ++ show (RExp u) ++ ")"
                            | otherwise = show (RExp u) ++"^"
showPowContextLeft (Var u) = u ++ "^"
showPowContextLeft u = "("++ show u ++")^"

showPowContextRight :: Exp -> String
showPowContextRight (RExp v) | denominator v /= 1 = "(" ++ show (RExp v) ++ ")"
                             | otherwise = show (RExp v)
showPowContextRight (Var v) = v
showPowContextRight v = "("++ show v ++")"

-- 4 TEST
-- (Sum (RExp 2) (Sum (RExp 3) (RExp 4)))
-- 2+3+4

-- (Sum (Sum (RExp 2) (RExp 3)) (RExp 4))
-- 2+3+4

-- (Sum (RExp 2) (Prod (RExp 3) (RExp 4)))
-- 2+3*4

-- (Prod (Sum (RExp 2) (RExp 3)) (RExp 4))
-- (2+3)*4

-- (Prod (RExp 4) (Sum (RExp 2) (RExp 3)))
-- 4*(2+3)

-- (Prod (RExp (2%3)) (RExp 3))
-- 2/3*3

-- (Pow (RExp (2%3)) (RExp 3))
-- (2/3)3

-- (Pow (Sum (RExp 1) (RExp 3)) (RExp 2))
-- (1+3)^2

-- (Pow (Prod (RExp 4) (RExp 3)) (RExp 2))
-- (4*3)^2

-- (Pow (Prod (RExp 4) (RExp 3)) (Sum (RExp 2) (RExp 4)))
-- (4*3)^(2+4)

-- (Pow (RExp (-1)) (RExp 2))
-- (-1)^2

-- (Pow (Pow (RExp 2) (RExp 3)) (RExp 2))
-- (2^3)^2

-- (Pow (RExp 2) (Pow (RExp 3) (RExp 2)))
-- 2^3^2


--------- 5

data Eqn = Eqn Exp Exp

fromExp :: Exp -> MPoly
fromExp (RExp n)   = Const n
fromExp (Var x)    = fromVar x
fromExp (Sum u v)  = fromExp u + fromExp v
fromExp (Prod u v) = fromExp u * fromExp v
fromExp (Pow u v)  = let (Const q)  = fromExp v
                         n          = numerator q
                         d          = denominator q
                     in
                         if d /= 1 then error "Fractional power"
                         else if n < 0 then error "Negative power"
                         else fromExp u ^ n

system :: [Eqn] -> [[Rational]]
system xs = toRationalList (convert xs) (sort (getParaList (convert xs) []))


convert :: [Eqn] -> [[(String, Rational)]]
convert = map convert'

convert' :: Eqn -> [(String, Rational)]
convert' (Eqn x y) = findPara (fromExp x) (fromExp y)

findPara :: MPoly -> MPoly -> [(String, Rational)]
findPara (ProdPlus (Const coe) (KVar v) (Const add)) (Const y)  = ("Result", y - add) : [(v, coe)]
findPara (ProdPlus (Const coe) (KVar v1) (Const add)) (ProdPlus (Const coe2) (KVar v2) (Const add2))
                                                    | v1 == v2  = ("Result", add2 - add):[(v2, coe - coe2)]
                                                    | otherwise = ("Result", add2 - add):((v1, coe) : [(v2, add)])
findPara (ProdPlus (Const coe) (KVar k) f) v = (k, coe) : findPara f v

getParaList :: [[(String, Rational)]] -> [String] -> [String]
getParaList xs rs = foldl (flip getParaList') rs xs

getParaList' :: [(String, Rational)] -> [String] -> [String]
getParaList' [] rs = rs
getParaList' ((var, value):xs) rs | var /= "Result" && isDuplicate var rs == False = getParaList' xs (var : rs)
                                  | otherwise = getParaList' xs rs

isDuplicate :: String -> [String] -> Bool
isDuplicate _ [] = False
isDuplicate x (y:ys) | x == y = True
                     | otherwise = isDuplicate x ys

toRationalList :: [[(String, Rational)]] -> [String] -> [[Rational]]
toRationalList [] _ = []
toRationalList (x:xs) vars = reverse (toRationalList' x vars [] []) : toRationalList xs vars

toRationalList' :: [(String, Rational)] -> [String] -> [Rational] -> [Rational] -> [Rational]
toRationalList' [] [] rs [x] = x : rs
toRationalList' [] (y:ys) rs rt = toRationalList' [] ys (0 % 1 : rs) rt
toRationalList' ((var, value):xs) (y:ys) rs rt | var == "Result" = toRationalList' xs (y:ys) rs (value : rt)
                                               | var == y = toRationalList' xs ys (value : rs) rt
                                               | otherwise = toRationalList' ((var, value):xs) ys (0 % 1 : rs) rt

-- 5 TEST
-- system [(Eqn (Prod (RExp 2) (Var "y")) (RExp 10))]
-- [[2 % 1,10 % 1]]

-- system [(Eqn (Var "y") (RExp 5)),(Eqn (Sum (Var "x") (Var "y")) (RExp 2))]
-- [[0 % 1,1 % 1,5 % 1],[1 % 1,1 % 1,2 % 1]]

-- system [(Eqn (Var "y") (RExp 5)),(Eqn (Sum (Var "x") (RExp 1)) (RExp 2))]
-- [[0 % 1,1 % 1,5 % 1],[1 % 1,0 % 1,1 % 1]]

-- system [(Eqn (Sum (Prod (RExp 2) (Var "x"))(Sum (Prod (RExp 3) (Var "z"))(Prod (RExp 3) (Var "y"))))(RExp 8)),(Eqn (Sum (Prod (RExp (-2)) (Var "z"))(Sum (Prod (RExp 3) (Var "y"))(Prod (RExp 2) (Var "x"))))(RExp 3)),(Eqn (Sum (Prod (RExp (-2)) (Var "y"))(Sum (Prod (RExp 4) (Var "x"))(Prod (RExp 2) (Var "z"))))(RExp 4))]
-- [[2 % 1,3 % 1,3 % 1,8 % 1],[2 % 1,3 % 1,(-2) % 1,3 % 1],[4 % 1,(-2) % 1,2 % 1,4 % 1]]



--------- 6
------ a

-- a TEST
-- (D (Pow (Var "x") (RExp 2)) "x")
-- D(x^2,x)

-- (Prod (RExp 5) (D (Pow (Var "x") (RExp 2)) "x"))
-- 5*D(x^2,x)



------ b
visitOnce :: (Exp -> Exp) -> Exp -> Exp
visitOnce f e@(RExp n) = e
visitOnce f e@(Var x)  = e
visitOnce f (Sum u v)  = f (Sum (visitOnce f u) (visitOnce  f v))
visitOnce f (Prod u v) = f (Prod (visitOnce f u) (visitOnce f v))
visitOnce f (Pow u v)  = f (Pow (visitOnce f u) (visitOnce f v))
visitOnce f (Ap Sin u) = Ap Sin (visitOnce f u)
visitOnce f (Ap Cos u) = Ap Cos (visitOnce f u)
visitOnce f (Ap Exp u) = Ap Exp (visitOnce f u)

visitUntilUnchanged :: (Exp -> Exp) -> Exp -> Exp
visitUntilUnchanged f tr = let new = visitOnce f tr
                           in if new == tr
                              then tr
                              else visitUntilUnchanged f new

simp2 :: Exp -> Exp
simp2 (Sum (RExp n) (RExp m))           = RExp (n+m)
simp2 (Sum (RExp n) v)                  = Sum v (RExp n)
simp2 (Sum u (RExp 0))                  = u
simp2 (Sum (Sum u (RExp n)) (RExp m))   = Sum u (RExp (n+m))
simp2 (Sum (Sum u (RExp n)) v)          = Sum (Sum u v) (RExp n)
simp2 (Sum u (Sum v w))                 = Sum (Sum u v) w
simp2 (Sum u v)
  | u == v                              = Prod (RExp 2) u
simp2 (Sum (Prod (RExp n) u) v)
  | u == v                              = Prod (RExp (n+1)) u
simp2 (Sum u (Prod (RExp n) v))
  | u == v                              = Prod (RExp (n+1)) u
simp2 (Sum (Prod (RExp n) u) (Prod (RExp m) v))
  | u == v                              = Prod (RExp (n+m)) u
simp2 (Prod (RExp n) (RExp m))          = RExp (n*m)
simp2 (Prod u (RExp n))                 = Prod (RExp n) u
simp2 (Prod (RExp 0) v)                 = RExp 0
simp2 (Prod (RExp 1) v)                 = v
simp2 (Prod (RExp n) (Prod (RExp m) v)) = Prod (RExp (n*m)) v
simp2 (Prod u (Prod (RExp n) v))        = Prod (RExp n) (Prod u v)
simp2 (Prod (Prod u v) w)               = Prod u (Prod v w)
simp2 (Prod (RExp n) (Sum u v))         = Sum (Prod (RExp n) u) (Prod (RExp n) v)
simp2 (Pow u (RExp 0))                  = RExp 1
simp2 (Pow u (RExp 1))                  = u
simp2 u                                 = u

simplify2 :: Exp -> Exp
simplify2 = visitUntilUnchanged simp2

simplify3 :: Exp -> Exp
simplify3 (D (RExp x) y) = RExp 0
simplify3 (D (Var x) y) | x == y = RExp 1
                        | otherwise = RExp 0
simplify3 (D (Sum x y) z) = simplify2 (Sum (simplify3 (D x z)) (simplify3 (D y z)))

simplify3 (D (Prod (RExp x) (Pow (Var u) (RExp v))) z) = simplify2 (Prod (RExp x) (simplify3 (D (Pow (Var u) (RExp v)) z)))
simplify3 (D (Prod (RExp x) (Pow (RExp u) (Var v))) z) = Prod (RExp x) (simplify3 (D (Pow (RExp u) (Var v)) z))
simplify3 (D (Prod (RExp x) y) z) = simplify2 (Prod (RExp x) (simplify3 (D y z)))
simplify3 (D (Prod x (RExp y)) z) = simplify2 (Prod (RExp y) (simplify3 (D x z)))
simplify3 (D (Prod (Var x) (Var y)) z) | x == y && y == z = Prod (RExp 2) (Var x)
                                       | x == z && y /= z = Var y
                                       | x /= z && y == z = Var x
                                       | otherwise = RExp 0
simplify3 (D (Prod x (Sum u v)) z) = Sum (simplify3 (D (Prod x u) z)) (simplify3 (D (Prod x v) z))
simplify3 (D (Prod (Var x) (Pow (RExp u) (Var v))) z) | x == v && v == z = Sum (Prod (Var x) (simplify3 (D (Pow (RExp u) (Var v)) z))) (Pow (RExp u) (Var v))
simplify3 (D (Pow (Var x) (RExp y)) z) | x == z && y - 1 > 1 = Prod (RExp y) (Pow (Var x) (RExp (y - 1)))
                                       | x == z && y - 1 == 1 = Prod (RExp y) (Var x)
                                       | otherwise = RExp 0
simplify3 (D (Pow (RExp x) (Var y)) z) | y == z = D (Pow (RExp x) (Var y)) z
                                       | otherwise = RExp 0
simplify3 (Prod x (D v z)) = simplify2 (Prod x (simplify3 (D v z)))

simplify3 (D (Ap Sin u) z)          = Ap Cos u
simplify3 (D (Ap Cos u) z)          = Prod (RExp (-1)) (Ap Sin u)
simplify3 (D (Ap Exp u) z)          | simplify3 (D u z) /= RExp 0 = simplify2 (Prod (simplify3 (D u z)) (Ap Exp u))
                                    | otherwise = RExp 0
simplify3 (D (Ap Ln u) z)           | u == RExp 1 = RExp 0
                                    | u == Var z  = Pow u (RExp (-1))
simplify3 (Int ((Prod x (Int u z))) z') | simplify3 (D x z) == u = Prod (RExp 0.5) (Pow x (RExp 2))
                                        | otherwise = RExp 0
simplify3 (Int u z) = simplify3 (D u z)


-- b TEST
-- simplify3 (D (RExp 2) "x")
-- 0
-- simplify3 (D (Var "x") "x")
-- 1

-- simplify3 (D (Var "y") "x")
-- 0

-- simplify3 (D (Sum (RExp 2) (Var "x")) "x")
-- 1

-- simplify3 (D (Sum (Var "x") (Var "x")) "x")
-- 2

-- simplify3 (D (Prod (RExp 5) (Var "x")) "x")
-- 5

--  simplify3 (D (Prod (Var "x") (Var "x")) "x")
-- 2*x

--  simplify3 (D (Prod (Var "x") (Sum (Var "x") (RExp 3))) "x")
-- 2*x+3

-- simplify3 (D (Pow (Var "x") (RExp 2)) "x")
-- 2*x

-- simplify3 (D (Prod (RExp 5) (Pow (Var "x") (RExp 2))) "x")
-- 10*x

-- simplify3 (D (Prod (RExp 5) (Pow (RExp 2) (Var "x"))) "x")
-- 5*D(2^x,x)

-- simplify3 (D (Prod (Var "x") (Pow (RExp 2) (Var "x"))) "x")
-- x*D(2^x,x)+2^x

-- simplify3 (Prod (RExp 5) (D (Pow (Var "x") (RExp 2)) "x"))
-- 10*x

--------- Extra 1
------ a
derivN :: (Fractional b) => (Double -> b) -> (Double -> b)
derivN f x = (f (x + 0.000000001) - f x) / 0.000000001

-- (derivN (\x->x^2)) 5
-- 10.00000082740371

------ b
-- 1
data Fractional a => DualNum a = DualNum (a, a) deriving (Eq, Show)

-- 2

instance (Num a, Fractional a) => Num (DualNum a) where
  DualNum (u, u') + DualNum (v, v') = DualNum (u + v, u' + v')
  negate (DualNum (u, u')) = DualNum (-u, -u')
  DualNum (u, u') * DualNum (v, v') = DualNum (u * v, u * v' - v * u')
  fromInteger x = DualNum (fromInteger x, 1)


instance (Num a, Fractional a) => Fractional (DualNum a) where
  x / DualNum (v, v') = DualNum (1/v, -v'/v^2)

-- 3
d :: (DualNum Double -> DualNum Double) -> (Double -> Double)
d f x = let DualNum(u,v) = (f (DualNum (x + 0.000000001, 1)) - f (DualNum (x, 1)))
            r = u / 0.000000001
        in d' r 1

d' :: Double -> Int -> Double
d' x n = fromIntegral (floor (x * t)) / t
    where t = 10^n

-- (d (\x->x^2)) 5
-- 10.0

-- 4
newtonImprove :: (DualNum Double -> DualNum Double) -> Double -> Double
newtonImprove f x = let DualNum (u, v) = derive' f x
                    in x - u / v


derive' :: (DualNum Double -> DualNum Double) -> (Double -> DualNum Double)
derive' f x = let DualNum(u,_) = (f (DualNum (x + 0.000000001, 1)) - f (DualNum (x, 1)))
                  DualNum(v,_) = f (DualNum (x, 1))
                  d = u / 0.000000001
              in DualNum (v, d' d 1)

-- (newtonImprove (\x->x^2-2)) 1
-- 1.5

-- 5 
curtApprox :: DualNum Double -> Seq Double
curtApprox (DualNum (x, y)) = Seq (iterate (newtonImprove (newLamba x)) 1 )

newLamba :: Double -> (DualNum Double -> DualNum Double)
newLamba x y = let DualNum (u, v) = y^3
               in DualNum (u - x, 1)

-- curtApprox 8
-- 1.0,3.3333333333333335,2.461350239128017,2.0795014668297687,2.002567901789967,1.999996701529111,2.000000027712757,...

-- 6
curt :: Double -> Double 
curt x = getResult x (curtApprox (DualNum (x, 1))) 

getResult :: Double -> Seq Double -> Double 
getResult i (Seq (x:xs)) | i > 0 = getResult (i - 1) (Seq xs)
                         | otherwise = x
-- curt 8
-- 2.0

----- Extra 1 TEST

-- (derivN (\x->x^2)) 5
-- 10.00000082740371

-- (d (\x->x^2)) 5
-- 10.0

-- (newtonImprove (\x->x^2-2)) 1
-- 1.5

-- >>> curtApprox 3
-- 1.0,3.3333333333333335,2.461350239128017,2.0795014668297687,2.002567901789967,1.999996701529111,2.000000027712757,...

-- curt 8
-- 2.0


--------- Extra 2
data Op = Sin |
          Cos |
          Exp |
          Ln deriving Eq


----- TEST
-- Ap Sin (Var "x")
-- Sin(x)

-- Ap Cos (Var "x")
-- Cos(x)

-- Ap Exp (Var "x")
-- e^(x)

-- Ap Ln (Var "x")
-- log(x)

-- Int (Var "x") "x"
-- Int(x)

-- simplify3 (Int (Var "x") "x")
-- 1

-- simplify3 (Int (RExp 1) "x")
-- 0

-- simplify3 (Int (Pow (Var "x") (RExp 2)) "x")
-- 2*x

-- simplify3 (Int (Ap Sin (Var "x")) "x")
-- Cos(x)

-- simplify3 (Int (Ap Cos (Var "x")) "x")
-- (-1)*Sin(x)

-- simplify3 (Int (Ap Exp (Var "x")) "x")
-- e^(x)

-- simplify3 (Int (Ap Ln (Var "x")) "x")
-- x^(-1)

-- simplify3 (Int (Prod (RExp 5) (Var "x")) "x")
-- 5

-- simplify3 (Int (Sum (RExp 5) (Var "x")) "x")
-- 1

------ a

----- TEST
-- simplify3 (Int (Prod (Ap Sin (Var "x")) (Int (Ap Cos (Var "x")) "x")) "x") 
-- 1/2*(Sin(x))^2

------ b

----- TEST
-- simplify3 (Int (Ap Sin (Pow (Var "x") (RExp 2))) "x")
-- Cos(x^2)
