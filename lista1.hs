-- ########## Q1 ########## --
-- verifica se um numero i é primo
-- divide todos os valores j antes de i para ver se há uma divisão exata
splitq p [] = ([],[])
splitq p (x:xs) | x < p = (x:a,b)
                | x >= p = (a,x:b)
                where (a,b) = splitq p xs

qsortEf :: [Int] -> [Int]
qsortEf [] = []
qsortEf (x:xs) = qsortEf e ++ [x] ++ qsortEf d
                 where (e,d) = splitq x xs               

ehprimo 0 _ = False
ehprimo 1 _ = False
ehprimo i j | (i == j) = True
            | ((mod i j) == 0) = False
            | otherwise = ehprimo i (j+1)

primos1 [] = ([],[])
primos1 (x:xs) | (ehprimo x 2) = (x:a , True:b)
               | otherwise = (a , False:b)
               where (a,b) = primos1 xs

duplicatas :: [Int] -> [Int]
duplicatas (x:[]) = [x]
duplicatas (x:y:xys) | (x==y) = duplicatas (y:xys)
                     | otherwise = x:duplicatas (y:xys)

primos l =  (duplicatas.(qsortEf.(fst.(primos1)))) l                

-- ########## Q2 ########## --
primo2 [] = 0
primo2 (x:xs) | (ehprimo x 2) = 1 + primo2 xs
              | otherwise = primo2 xs

primoEntre a b = primo2 [a..b]

-- ########## Q3 ########## --
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

-- ########## Q4 ########## --
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

-- ########## Q5 ########## --
digitos 0 = []
digitos x = (mod x 10):(digitos (div x 10))

somaDig x = soma (digitos x)

-- ########## Q6 ########## --
soma l = foldr (+) 0 l 

somaLista l = map soma l



