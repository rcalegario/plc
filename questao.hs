-- module L1Q1 where

-- divisao de uma lista usada no quicksort
splitq :: Ord a => a -> [a] -> ([a],[a])
splitq p [] = ([],[])
splitq p (x:xs) | x < p = (x:a,b)
                | x >= p = (a,x:b)
                where (a,b) = splitq p xs

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort e ++ [x] ++ qsort d
               where (e,d) = splitq x xs

--ehprimo :: Int -> Int -> Bool
--ehprimo 0 _ = False
--ehprimo 1 _ = False
--ehprimo p j | (p == j) = True
--            | ((mod p j) == 0) = False
--            | otherwise = ehprimo p (j+1)        

-- retira as diplicatas de uma lista 
duplicatas :: Eq a => [a] -> [a]
duplicatas (x:[]) = [x]
duplicatas (x:y:xys) | (x==y) = duplicatas (y:xys)
                     | otherwise = x:duplicatas (y:xys)  

primos :: [Int] -> ([Int],[Bool])
primos l = (duplicatas(qsort ([x | x <- l, ehprimo x 2])), [ehprimo x 2 | x <- l]) 

-- ############################################### --
-- module L1Q2 where
ehprimo :: Int -> Int -> Bool
ehprimo 0 _ = False
ehprimo 1 _ = False
ehprimo p j | (p == j) = True
            | ((mod p j) == 0) = False
            | otherwise = ehprimo p (j+1) 

primo2 [] = 0
primo2 (x:xs) | (ehprimo x 2) = 1 + primo2 xs
              | otherwise = primo2 xs

primoEntre a b = primo2 [a..b]

-- ############################################### --
-- module L1Q3 where
--ehprimo :: Int -> Int -> Bool
--ehprimo 0 _ = False
--ehprimo 1 _ = False
--ehprimo p j | (p == j) = True
--            | ((mod p j) == 0) = False
--            | otherwise = ehprimo p (j+1) 

primo3 [] = []
primo3 (x:xs) | (ehprimo x 2) = x:primo3 xs
              | otherwise = primo3 xs

somaPrimo a [] c = (0,0)
somaPrimo a (b:bs) c | ((a + b) == c) = (a,b)
                     | otherwise = somaPrimo a bs c

find [] _ = (0,0)
find (x:xs) y | a /= (0,0) = a
              | otherwise = find xs y
              where a = somaPrimo x xs y

findPrimes x = find (primo3 [2..x]) x 


-- ############################################### --
-- module L1Q4 where
split [] = ([],[])
split [x] = ([x],[])
split (x:y:zs) = (x:a,y:b)
               where (a,b) = split zs

merge x [] = x
merge [] y = y
merge (x:xs) (y:ys) | x <= y = x:(merge xs (y:ys))
                    | otherwise = y:(merge (x:xs) ys)

mergesort [] = []
mergesort [x] = [x]
mergesort l = merge (mergesort a) (mergesort b)
            where (a,b) = split l


-- ############################################### --
-- module L1Q5 where
soma l = foldr (+) 0 l 

digitos 0 = []
digitos x = (mod x 10):(digitos (div x 10))

somaDig x = soma (digitos x)

-- ############################################### --
-- module L1Q6 where
-- soma l = foldr (+) 0 l 
somaLista l = map soma l