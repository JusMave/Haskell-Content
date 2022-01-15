import Data.Char
import qualified Data.Maybe
import Scanner
import Series
import MPoly
import Data.Ratio
import Data.List

--------- 1
---- a

data RegExp sigma = RegEmpty                               |
                    RegEpsilon                             |
                    RegSym  sigma                          |
                    RegOr   (RegExp sigma)  (RegExp sigma) |
                    RegSeq  (RegExp sigma)  (RegExp sigma) |
                    RegStar (RegExp sigma)                 |
                    LetterClass                            |
                    DigitClass deriving (Eq, Show)

---- b
nullable :: RegExp a -> Bool
nullable RegEmpty     = False
nullable RegEpsilon   = True
nullable (RegSym x)   = False
nullable (RegOr x y)  = nullable x || nullable y
nullable (RegSeq x y) = nullable x && nullable y
nullable (RegStar x)  = True

nullable LetterClass  = False
nullable DigitClass   = False

---- c
nu :: RegExp a -> RegExp a
nu re | nullable re = RegEpsilon
      | otherwise   = RegEmpty

---- d
-- deriv :: Eq Char => Char -> RegExp Char -> RegExp Char
deriv :: Char -> RegExp Char -> RegExp Char
deriv alpha RegEmpty    = RegEmpty
deriv alpha RegEpsilon  = RegEmpty
deriv alpha (RegSym x)  | alpha == x = RegEpsilon
                        | otherwise = RegEmpty
deriv alpha (RegOr x y) = RegOr (deriv alpha x) (deriv alpha y)
deriv alpha (RegSeq x y) = RegOr (RegSeq (deriv alpha x) y) (RegSeq (nu x) (deriv alpha y))
deriv alpha (RegStar x) = RegSeq (deriv alpha x) (RegStar x)
deriv alpha LetterClass | isAlpha alpha = RegEpsilon
                        | otherwise = RegEmpty
deriv alpha DigitClass  | isDigit alpha = RegEpsilon
                        | otherwise = RegEmpty
---- e
-- match :: Eq a => [a] -> RegExp a -> Bool
match :: [Char] -> RegExp Char -> Bool
match [] rs = nullable rs
match (x:xs) rs = match xs (deriv x rs)


---- TEST Case

-- match "" RegEmpty
-- False

-- match "a" RegEmpty
-- False

-- match "" RegEpsilon
-- True

-- match "a" RegEpsilon
-- False

-- match "" (RegSym 'a')
-- False

-- match "a" (RegSym 'a')
-- True

-- match "b" (RegSym 'a')
-- False

-- match "aa" (RegSym 'a')
-- False

-- match "" (RegOr (RegSym 'a') (RegSym 'b'))
-- False

-- match "a" (RegOr (RegSym 'a') (RegSym 'b'))
-- True

-- match "b" (RegOr (RegSym 'a') (RegSym 'b'))
-- True

-- match "ab" (RegOr (RegSym 'a') (RegSym 'b'))
-- False

-- match "" (RegSeq (RegSym 'a') (RegSym 'b'))
-- False

-- match "a" (RegSeq (RegSym 'a') (RegSym 'b'))
-- False

-- match "b" (RegSeq (RegSym 'a') (RegSym 'b'))
-- False

-- match "ab" (RegSeq (RegSym 'a') (RegSym 'b'))
-- True

-- match "ba" (RegSeq (RegSym 'a') (RegSym 'b'))
-- False

-- match "aba" (RegSeq (RegSym 'a') (RegSym 'b'))
-- False

-- match ""  (RegStar (RegSym 'a'))
-- True

-- match "a" (RegStar (RegSym 'a'))
-- True

-- match "aa" (RegStar (RegSym 'a'))
-- True

-- match "b" (RegStar (RegSym 'a'))
-- False

-- match "ab" (RegStar (RegSym 'a'))
-- False

-- match "abb" (RegSeq (RegSym 'a') (RegStar (RegSym 'b')))
-- True

-- match "aba" (RegSeq (RegSym 'a') (RegStar (RegSym 'b')))
-- False


--------- 2
---- a
empty :: RegExp a -> Bool
empty RegEmpty     = True
empty RegEpsilon   = False
empty (RegSym x)   = False
empty (RegOr x y)  = empty x && empty y
empty (RegSeq x y) = empty x || empty y
empty (RegStar x)  = empty x
empty LetterClass  = False
empty DigitClass   = False

---- b
-- data Token = Simple SimpleToken     |
--              Compound CompoundToken |
--              LexError String        deriving (Eq, Show)

-- data SimpleToken = EOF   |
--                    COMMA |
--                    PLUS  |
--                    MINUS |
--                    STAR  |
--                    SLASH |
--                    EQ1   |
--                    OP    |
--                    CP    |
--                    LET   |
--                    IN
--                    deriving (Eq, Show)

-- data CompoundToken = Id String | Num Integer deriving (Eq, Show)

---- c
type RegVec = [(RegExp Char, String -> Token)]

---- d
derivVec :: Char -> RegVec -> RegVec
derivVec _ [] = []
derivVec x ((r, f):ys) = (deriv x r, f):derivVec x ys

---- e
emptyVec :: RegVec -> Bool
emptyVec [] = True
emptyVec ((r,f):xs) | empty r = emptyVec xs
                    | otherwise = False

---- f
nullableInVec :: RegVec -> Maybe (String -> Token)
nullableInVec [] = Nothing
nullableInVec ((rv, f):xs) | nullable rv = Just f
                           | otherwise = nullableInVec xs


---- g
findNullable :: [([Char], RegVec, [Char])] -> (Token, String)
findNullable [] = (LexError "Unidentified text.", "")
findNullable ((s1, r, s2):xs) | Data.Maybe.isJust (nullableInVec r) =
                                    let (Just f) = nullableInVec r
                                    in (f (reverseString s1 []), s2)
                               | otherwise = findNullable xs

---- h
splitInputHelper :: String -> RegVec -> [Char] -> [([Char], RegVec, [Char])] -> (Token, String)
splitInputHelper [] _ _ stack = findNullable stack
splitInputHelper (z:zs) rv chars stack | emptyVec (derivVec z rv) = findNullable stack
                                       | otherwise = splitInputHelper zs (derivVec z rv) (z:chars) ((z:chars, (derivVec z rv), zs):stack)

splitInput :: String -> RegVec -> (Token, String)
splitInput s rv = splitInputHelper s rv [] []

reverseString :: String -> String -> String
reverseString xs ys = foldl (flip (:)) ys xs


---- i
makeTokenStreamFromString :: RegVec -> String -> [Token]
makeTokenStreamFromString _ [] = [Simple EOF]
makeTokenStreamFromString rv (x:xs) | Data.Char.isSpace x =  makeTokenStreamFromString rv xs
                                    | otherwise = let (token, str) = splitInput (x:xs) rv
                                                  in  token : makeTokenStreamFromString rv str

---- j
regOrFromList :: [RegExp sigma] -> RegExp sigma
regOrFromList [x] = x
regOrFromList (x:xs) = RegOr x (regOrFromList xs)

-- regOrFromList  [RegSym d | d <- ['0' .. '9']]
-- RegOr (RegSym '0') 
--       (RegOr (RegSym '1') 
--              (RegOr (RegSym '2') 
--                     (RegOr (RegSym '3') 
--                            (RegOr (RegSym '4') 
--                                   (RegOr (RegSym '5') 
--                                          (RegOr (RegSym '6') 
--                                                 (RegOr (RegSym '7') 
--                                                        (RegOr (RegSym '8') 
--                                                               (RegSym '9')))))))))


-- TEST
tokenStreamFromString :: String -> [Token]
tokenStreamFromString =
      makeTokenStreamFromString [(RegSym ',', \s->Simple COMMA),
                                 (RegSym '+', \s->Simple PLUS),
                                 (RegSym '-', \s->Simple MINUS),
                                 (RegSym '*', \s->Simple STAR),
                                 (RegSym '/', \s->Simple SLASH),
                                 (RegSym '=', \s->Simple EQ1),
                                 (RegSym '(', \s->Simple OP),
                                 (RegSym ')', \s->Simple CP),
                                 (RegSeq (RegSym 'l')
                                         (RegSeq (RegSym 'e') (RegSym 't')),
                                    \s->Simple LET),
                                 (RegSeq (RegSym 'i') (RegSym 'n'),
                                    \s->Simple IN),
                                 (RegSeq LetterClass (RegStar LetterClass),
                                    Compound . Id),
                                 (RegSeq DigitClass (RegStar DigitClass),
                                    Compound . Num . read)]

-- >>> tokenStreamFromString "2+3"
-- [Compound (Num 2),Simple PLUS,Compound (Num 3),Simple EOF]

-- >>> tokenStreamFromString "letter let"
-- [Compound (Id "letter"),Simple LET,Simple EOF]

-- >>> tokenStreamFromString "let xx = 21,  yy = 99 in xx * yy"
-- [Simple LET,Compound (Id "xx"),Simple EQ1,Compound (Num 21),Simple COMMA,Compound (Id "yy"),Simple EQ1,Compound (Num 99),Simple IN,Compound (Id "xx"),Simple STAR,Compound (Id "yy"),Simple EOF]
-- 

-- >>> tokenStreamFromString "2˜3"
-- [Compound (Num 2),LexError "Unidentified text.",Simple EOF]

--------- 3


data Exp = RExp Rational   |
           Var String     |
           Sum Exp Exp    |
           Diff Exp Exp   |
           Prod Exp Exp   |
           Quo Exp Exp    |
           Neg Exp        |
         --   Let ExpSeq Exp |
           ParseError String deriving (Eq, Show)

-- data ExpSeq = Seq [Binding] deriving (Eq, Show)
-- data Binding = Bind String Exp deriving (Eq, Show)

data EqnSeq = Seq [Eqn] | ParseEqnSeqError String deriving (Eq, Show)
data Eqn = Eqn Exp Exp deriving (Eq, Show)

stringFromToken :: Token -> String
stringFromToken (Compound (Id s)) = s

integerFromToken :: Token -> Rational
integerFromToken (Compound (Num n)) = toRational n

-- Grammar
-- E ::= let D in E 
--   ::= M
-- D ::= B, D
--   ::= B
-- B ::= id = E
-- M ::= T M'
-- M'::= + T M'
--   ::= - T M'
--   ::= epsilon
-- T ::= F T'
-- T'::= * F T'
--   ::= / F T'
--   ::= epsilon
-- F ::= id
-- F ::= num
-- F ::= - F
-- F ::= ( E )

newtype Parser a = Parser ([Token] -> [(a, [Token])])

unWrap (Parser f) = f

instance Monad Parser where
  return a = Parser(\ts->[(a, ts)])
  p >>= f  = Parser(\ts->[(b, ts2) | (a, ts1) <- unWrap p ts, (b, ts2) <- unWrap (f a) ts1])

fail2 :: p -> Parser a
fail2 s   = Parser(\ts-> [])

instance Applicative Parser where
    pure  = return
    mf <*> ma = do f <- mf
                   a <- ma
                   return (f a)

instance Functor Parser where
  fmap g fx = (pure g) <*> fx



-- item makes sense for "token" Parsers
item :: Parser Token
item = Parser(\ts->case ts of [] -> []; t:ts1 -> [(t,ts1)])

parserFilter :: Parser b -> (b -> Bool) -> Parser b
parserFilter parser p = do {a <- parser; if p a then return a else fail2 "Parser"}

literal :: Token -> Parser Token
literal t = parserFilter item (==t)

variable :: Parser Token
variable =  parserFilter item (\tok->case tok of (Compound (Id _)) -> True; _ -> False)

number :: Parser Token
number =  parserFilter item (\tok->case tok of (Compound (Num _)) -> True; _ -> False)

(+++) :: Parser a -> Parser a -> Parser a
p1 +++ p2 = Parser(\ts-> (unWrap p1 ts) ++ (unWrap p2 ts))

getExp :: Parser Exp
getExp = getMathExp

getDefs :: Parser EqnSeq
getDefs = getDefs2 []

getDefs2 :: [Eqn] -> Parser EqnSeq
getDefs2 revBinds =
  do binding <- getDef
     (do tok <- literal (Simple COMMA)
         getDefs2 (binding:revBinds))
       +++
      return (Seq (reverse (binding:revBinds)))

getDef :: Parser Eqn
getDef =
  do
     exp1 <- getExp
     tok <- (literal (Simple EQ1))
     exp2 <- getExp
     return (Eqn exp1 exp2)

getMathExp :: Parser Exp
getMathExp =
  do term <- getTerm
     getMathExp' term

getMathExp' :: Exp -> Parser Exp
getMathExp' term =
  (do tok <- (literal (Simple PLUS))
      term2 <- getTerm
      getMathExp' (Sum term term2))
  +++
  (do tok <- (literal (Simple MINUS))
      term2 <- getTerm
      getMathExp' (Diff term term2))
  +++
   (return term)

getTerm :: Parser Exp
getTerm =
  do factor <- getFactor
     getTerm' factor

getTerm' :: Exp -> Parser Exp
getTerm' factor =
  (do tok <- (literal (Simple STAR))
      factor2 <- getFactor
      getTerm' (Prod factor factor2))
  +++
  (do tok <- (literal (Simple SLASH))
      factor2 <- getFactor
      getTerm' (Quo factor factor2))
  +++
   (return factor)

getFactor :: Parser Exp
getFactor =
  (do vtok <- variable
      return (Var (stringFromToken vtok)))
  +++
  (do ntok <- number
      return (RExp (integerFromToken ntok)))
  +++
  (do tok <- (literal (Simple MINUS))
      factor <- getFactor
      return (Neg factor))
  +++
  (do tok <- (literal (Simple OP))
      exp <- getExp
      tok <- (literal (Simple CP))
      return exp)

parse :: [Token] -> EqnSeq
parse ts =
  case unWrap getDefs ts of
    []            -> ParseEqnSeqError "Bad input"
    (exp, ts1):ps -> if isEmptyTokenStream ts1
                     then exp
                     else ParseEqnSeqError "Unconsumed input"

parseString :: String -> EqnSeq
parseString = parse . Scanner.tokenStreamFromString

-- >>> parse (Scanner.tokenStreamFromString "w=1/2*w+20")
-- Seq [Eqn (Var "w") (Sum (Prod (Quo (RExp (1 % 1)) (RExp (2 % 1))) (Var "w")) (RExp (20 % 1)))]

-- >>> parse (Scanner.tokenStreamFromString "2*y+x=10, 2*x+1=9")
-- Seq [Eqn (Sum (Prod (RExp (2 % 1)) (Var "y")) (Var "x")) (RExp (10 % 1)),
--      Eqn (Sum (Prod (RExp (2 % 1)) (Var "x")) (RExp (1 % 1))) (RExp (9 % 1))]

-- >>> parse (Scanner.tokenStreamFromString "w=/2*w+20")
-- ParseEqnSeqError "Bad input"

--------- Extra 1
fromExp :: Exp -> MPoly
fromExp (RExp n)   = Const n
fromExp (Var x)    = fromVar x
fromExp (Sum u v)  = fromExp u + fromExp v
fromExp (Diff u v) = fromExp u - fromExp v
fromExp (Prod u v) = fromExp u * fromExp v
fromExp (Quo (RExp u) (RExp v)) = Const (u / v)

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
toRationalList (x:xs) vars = sortResult (toRationalList' (sort x) vars [] []) : toRationalList xs vars

toRationalList' :: [(String, Rational)] -> [String] -> [Rational] -> [Rational] -> [Rational]
toRationalList' [] [] rs [x] = x : rs
toRationalList' [] (y:ys) rs rt = toRationalList' [] ys (0 % 1 : rs) rt
toRationalList' ((var, value):xs) (y:ys) rs rt | var == "Result" = toRationalList' xs (y:ys) rs (value : rt)
                                               | var == y = toRationalList' xs ys (value : rs) rt
                                               | otherwise = toRationalList' ((var, value):xs) ys (0 % 1 : rs) rt

sortResult :: [Rational] -> [Rational]
sortResult (x:xs) = xs ++ [x]

-- solve system

triangulate :: (Eq a, Num a, Fractional a) => [[a]] -> [[a]]
triangulate [] = []
triangulate (x:xs) | checkFirstIndex x 0 = triangulate (nonZeroFirst (x:xs))
                   | otherwise = x:triangulate(subAll x xs)

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

subAll :: (Fractional a, Eq a) => [a] -> [[a]] -> [[a]]
subAll _ [] = []
subAll x (y:ys) = subScale x y : subAll x ys

sub :: Num a => [a] -> [a] -> [a]
sub [] [] = []
sub (x:xs) (y:ys) = (x - y) : sub xs ys

scaleList :: Num a => a -> [a] -> [a]
scaleList _ [] = []
scaleList t (x:xs) = (t * x) : scaleList t xs

subScale :: (Fractional a) => [a] -> [a] -> [a]
subScale (x:xs) (y:ys) = drop 1 (sub (y:ys) (scaleList (y / x) (x:xs)))

dot :: Num a => [a] -> [a] -> a
dot [] [] = 0
dot xs [] = 0
dot [] ys = 0
dot (x:xs) (y:ys) = x * y + dot xs ys

solveLine :: Fractional a => [a] -> [a] -> a
solveLine (x:xs) ys = (lastElement (x:xs) - dot xs ys) / x

lastElement :: [a] -> a
lastElement [x] = x
lastElement (x:xs) = lastElement xs

solveTriangular :: Fractional a => [[a]] -> [a]
solveTriangular [x] = [solveLine x [0]]
solveTriangular (x:xs) = solveLine x (solveTriangular xs) : solveTriangular xs

solveSystem :: (Fractional a, Eq a) => [[a]] -> [a]
solveSystem xs = solveTriangular (triangulate xs)

readEqus :: String -> String -> IO ()
readEqus prompt sentinel = do
                              putStr prompt
                              str <- getLine
                              if str == sentinel then putStrLn sentinel
                              else do printResult (readEqus' (parseString str))
                                      readEqus prompt sentinel

printResult :: [(String, Integer)] -> IO ()
printResult ((var, value) : xs) = do putStr (var ++ " = " ++ show value)
                                     if not (null xs) then do putStr ", "
                                                              printResult xs
                                     else putStrLn ""


readEqus' :: EqnSeq -> [(String, Integer)]
readEqus' (Seq xs) = combine (sort (getParaList (convert xs) [])) (calculate xs)

calculate :: [Eqn] -> [Integer]
calculate xs = toIntList (solveSystem (system xs))

toIntList :: [Rational] -> [Integer]
toIntList = map numerator

combine :: [String] -> [Integer] -> [(String, Integer)]
combine [] [] = []
combine (x:xs) (y:ys) = (x, y) : combine xs ys

interface :: IO ()
interface = do putStrLn "Enter a system of equations."
               putStrLn "When finished, type ’done’."
               eqnSeq <- readEqus "System: " "done"
               putStr ""

-- *Main> interface
-- Enter a system of equations.
-- When finished, type ’done’.
-- System: w = 1/2*w + 20
-- w = 40
-- System: x + y = 5, y - x = 1
-- x = 3, y = 2
