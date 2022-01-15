import Distribution.Simple.Utils (xargs)

perm :: [a] -> [[a]]
perm [] = []
perm (x:xs) = comb [[x]] xs

comb :: [[a]] -> [a] -> [[a]]
comb xs [] = xs
comb [] _ = []
comb (x:xs) (z:zs) = comb (insert [] x z) zs ++ comb xs (z:zs)

insert :: [a] -> [a] -> a -> [[a]]
insert xs [] z = [xs++[z]]
insert xs (y:ys) z = (xs ++ (z:y:ys)) : insert (xs++[y]) ys z

-- >>> insert [] [1,3,4] 2
-- [[2,1,3,4],[1,2,3,4],[1,3,2,4],[1,3,4,2]]


--all' = \x -> ((+1) x, (+1) x)
{-
all' :: a -> a
all' = pair id (const 0)
-}
-- >>> all' 1
-- Couldn't match expected type: t0 -> t
--             with actual type: (a0 -> a0, b0 -> Integer)


equals        :: Eq a => [a] -> Bool
equals []     = False
equals (x:xs) = null xs || equals' x xs
  where
    equals' x []     = True
    equals' x (y:ys) = x == y && equals' x ys

-- >>> equals [0,1]
-- False


pair       :: (a -> a) -> (a -> a) -> (a -> [a])
pair f1 f2 x = [f1 x, f2 x]

-- >>> pair (id) (const 0) 1
-- [1,0]

zero :: Integer -> Bool
zero = equals . pair id (const 0)

decr :: Integer -> Integer
decr = difference . pair id (const 1)

-- >>> zero 10 
-- False

difference        :: Eq a => Num a => [a] -> a
difference (x:xs) | null xs = x
                  | otherwise = x - difference xs


imp            :: (a -> Bool) -> (a -> a) -> (a -> a) -> (a -> a)
imp f1 f2 f3 x | f1 x = f2 x
               | otherwise = f3 x

fact :: Integer -> Integer
fact = imp zero (const 1) (product . (pair id (fact . decr)))

-- >>> fact 5
-- 120


main = do
    print ()
