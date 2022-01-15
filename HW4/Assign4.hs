

------- 1

---- a
index_a :: Char -> String -> Maybe Int
index_a _ [] = Nothing
index_a x (y:ys) | x /= y    = fmap (1+) (index_a x ys)
                 | otherwise = Just 0

-- >>> index_a 'x' "qrsxyz"
-- Just 3

-- >>> index_a 'x' "qrsyz"
-- Nothing

---- b
index_b :: Char -> String -> Maybe Int
index_b _ [] = Nothing
index_b x (y:ys) | x /= y    = pure (1+) <*> index_b x ys
                 | otherwise = Just 0
-- >>> index_b 'x' "qrsxyz"
-- Just 3

-- >>> index_b 'x' "qrsyz"
-- Nothing

---- c

index_c :: Char -> String -> (Int -> Int) -> Maybe Int
index_c x [] k = Nothing
index_c x (y:ys) k | x /= y = index_c x ys (\v->k (1+v))
                   | otherwise = Just (k 0)

-- >>> index_c 'x' "qrsxyz" id
-- Just 3

-- >>> index_c 'x' "qrsyz" id
-- Nothing

---- d

newtype K r a = K ((a -> r) -> r)

(<<<) :: K r a -> (a -> r) -> r
(K f) <<< k = f k

instance Monad (K r) where
    return v = K(\k->k v)
    m >>= f  = K(\k->m <<< (\a->(f a) <<< k))

instance Applicative (K r) where
    pure  = return
    mf <*> ma = do f <- mf
                   a <- ma
                   return (f a)

instance Functor (K r) where
  fmap g fx = (pure g) <*> fx

abortWith :: r -> K r a
abortWith v = K (\k -> v)

index_d :: Char -> [Char] -> K (Maybe Int) a
index_d x [] = abortWith Nothing
index_d x (y:ys) | x /= y = K(\k -> index_c x ys (\v->(1+v)))
                 | otherwise = K(\k -> Just 0)

topIndex_d :: Char -> [Char] -> Maybe Int
topIndex_d x xs = (index_d x xs) <<< id

-- >>> index_d 'x' "qrsxyz" <<< id
-- Just 3

-- >>> index_d 'x' "qrsyz" <<< id
-- Nothing

-- >>> topIndex_d 'x' "qrsxyz"
-- Just 3

-- >>> topIndex_d 'x' "qrsyz"
-- Nothing

---- e

index_e :: Char -> [Char] -> Maybe Int
index_e _ [] = Nothing
index_e x (y:ys) | x /= y    = index_e x ys >>= (\x -> return (x + 1))
                 | otherwise = return 0

-- >>> index_e 'x' "qrsxyz"
-- Just 3

-- >>> index_e 'x' "qrsyz" 
-- Nothing

---- f

index_f :: Eq t => t -> [t] -> Maybe Int
index_f x [] = Nothing
index_f x (y:ys) | x /= y = do z <- index_f x ys
                               return (z + 1)
                 | otherwise = return 0

-- >>> index_f 'x' "qrsxyz"
-- Just 3

-- >>> index_f 'x' "qrsyz"
-- Nothing

------- 2

meetAndGreet :: IO ()
meetAndGreet = do putStr "What is your name? "
                  name <- getLine
                  putStrLn ("Hello " ++ name)

------- 3

readDoubles :: String -> String -> IO [Double]
readDoubles prompt sentinel = readDoubles' prompt sentinel []

readDoubles' :: String -> String -> [Double] -> IO [Double]
readDoubles' prompt sentinel state  = do
    putStr prompt
    value <- getLine
    if value == sentinel then do return state
    else if value == "" || value == " " then readDoubles' prompt sentinel state
    else do readDoubles' prompt sentinel (doubleParser value:state)

doubleParser :: String -> Double
doubleParser x = read x :: Double

average :: [Double] -> IO Double
average xs = average' xs 0 0

average' :: [Double] -> Double -> Double -> IO Double
average' [] y z = return (y / z)
average' (x:xs) y z = average' xs (x + y) (z + 1)

interface :: IO ()
interface = do putStrLn "Enter some numbers."
               putStrLn "When finished, type ’done’."
               doubles <- readDoubles "Enter a number: " "done"
               putStr "The average is "
               mean <- average doubles
               putStrLn (show mean)
               (max, min) <- maxAndMin doubles 0 0 0
               putStr "The maximum is "
               putStrLn (show max)
               putStr "The minimum is "
               putStrLn (show min)

maxAndMin :: [Double] -> Double -> Double -> Int -> IO (Double, Double)
maxAndMin [] max min i = return (max, min)
maxAndMin (x:xs) max min i | i == 0  = maxAndMin xs x x 1
                           | x > max = maxAndMin xs x min (i + 1)
                           | x < min = maxAndMin xs max x (i + 1)
                           | otherwise = maxAndMin xs max min (i + 1)

-- >>> interface
-- Enter some numbers.
-- When finished, type ’done’.
-- Enter a number: 2
-- Enter a number: 1
-- Enter a number: 3
-- Enter a number: done
-- The average is 2.0
-- The maximum is 3.0
-- The minimum is 1.0

------- Extra 1

cp :: String -> String -> IO String
cp fileName1 fileName2 = do contents <- readFile fileName1
                            writeFile fileName2 contents
                            return contents
