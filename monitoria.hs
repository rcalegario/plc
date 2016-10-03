-- Q1a
repeticao :: Eq t => t -> [t] -> Bool
repeticao a [] = False
repeticao a (x:xs) | a == x = True
                   | otherwise = repeticao a xs 

membro :: Eq t => [t] -> [t]
membro [] = []
membro (x:xs) | repeticao x xs = membro xs
              | otherwise = x:membro xs

questao1 :: Eq t => [t] -> [t] -> [t]
questao1 a b = membro (a++b)

-- Q1b
cartesiano :: [t] -> [t] -> [(t,t)]
cartesiano a b = [(x, y) | x <- a, y <- b]

questao2 :: [t] -> [t] -> [(t,t)]
questao2 a b = membro (cartesiano a b)

-- ##################################################### --
-- #################### MONITORIA 2 #################### --
-- ##################################################### --

-- map (+) :: ??
-- map :: (a -> b) -> [a] -> [b]
-- (+) :: Int -> Int -> Int
-- comparando map com (+) 
---- (a -> b) # (Int -> (Int -> Int))
---- a = Int # b = (Int -> Int)
-- map (+) :: [Int] -> [Int -> Int]

-- ##################################################### --
-- filter (>0).map (+1)
-- ##################################################### --
---- map (+1) :: ??
---- map :: (a -> b) -> [a] -> [b]
---- (+1) :: Int -> Int
---- comparando map com (+1) 
------ (a -> b) # (Int -> Int)
------ a = Int # b = Int
---- map (+1) :: [Int] -> [Int]

---- filter (>0) :: ??
---- filter :: (b -> Bool) -> [b] -> [b]
---- (>0) :: Int -> Bool
---- comparando filter com (>0) 
------ (b -> Bool) # (Int -> Bool)
------ a = Int 
---- filter (>0) :: [Int] -> [Int]

-- (.) :: (y -> z) -> (x -> y) -> x -> z
-- comparando (.) com filter (>0)
---- (y -> z) # [Int] -> [Int]
---- y = [Int] # z = [Int]
-- comparando (.) com map (+1)
---- (x -> y) # [Int] -> [Int]
---- x = [Int] # y = [Int] 
---- se y = [a] e y = [Int] entao a = Int
-- filter (>0).map (+1) :: [Int] -> [Int]

-- ##################################################### --
-- map.filter
-- ##################################################### --
-- map :: (a -> b) -> [a] -> [b]
-- filter :: (c -> Bool) -> [c] -> [c]
-- (.) :: (y -> z) -> (x -> y) -> x -> z
-- comparando map com (.)
---- (y -> z) # (a -> b) -> [a] -> [b]
---- y = (a -> b) # z = ([a] -> [b])
-- comparando filter com (.)
---- (x -> y) # (c -> Bool) -> [c] -> [c]
---- x = (c -> Bool) # y = ([c] -> [c])
---- se y = (a -> b) e y = ([c] -> [c]) entao a = [c] e b = [c]
-- map.filter :: (c -> Bool) -> ([[c]] -> [[c]])

-- ##################################################### --
-- map.foldr
-- ##################################################### --
-- map :: (a -> b) -> [a] -> [b]
-- foldr :: (c -> d -> d) -> d -> [c] -> d
-- (.) :: (y -> z) -> (x -> y) -> x -> z
-- comparando map com (.)
---- (y -> z) # (a -> b) -> [a] -> [b]
---- y = (a -> b) # z = ([a] -> [b])
-- comparando foldr com (.)
---- (x -> y) # (c -> d -> d) -> d -> [c] -> d
---- x = (c -> d -> d) # y = (d -> [c] -> d)
---- se y = (a -> b) e y = (d -> [c] -> d) entao a = d e b = ([c] -> d)
-- map.foldr :: (c -> d -> d) -> ([d] ->[[c] -> d])

-- ##################################################### --
-- Dada uma arvore binaria, devolver uma funcao que, dada uma arvore binaria, verifica se as arvores sao isomorficas
-- ##################################################### --

Tree 
































































