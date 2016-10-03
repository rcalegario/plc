-- concatenacao de duas listas
conca :: x -> y -> z
conca :: [w] -> y -> y
conca :: [w] -> y -> y   { x :: w , xs :: [w]}
conca :: [w] -> [w] -> [w] -- por saber que ":" é sempre uma lista, então haskell sabre que o retorno é uma lista

conca [] b = b
conca (x:xs) b = x:(conca xs b)

-- reverte a ordem da lista
rev [] = []
rev (x:xs) = conca (rev xs) [x]
