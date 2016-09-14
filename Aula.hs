-- LISTA

-- soma de todos os valores da lista
soma [] = 0
soma (x:xs) = x + soma xs

-- tamanho da lista
tamLista [] = 0
tamLista (x:xs) = 1 + tamLista xs

-- busca de um elemento da lista por indice
indice [] i = []
indice (x:xs) 0 = [x]
indice (x:xs) i = indice xs (i-1)

-- busca maior elemento de um lista de inteiros 
maior [] = 0
maior (x:[]) = x -- permite inteiros negativos
-- maior (x:xs) = if (x > maior xs) then x else maior xs 
maior (x:xs) | x > maior xs = x 
             | otherwise = maior xs 

-- conjuntos
-- vefica se um inteiro é encontrado em uma lista
member e [] = False
member e (x:xs) | x == e = True
                | otherwise = member e xs

-- uniao
union [] l = l
union (x:xs) l | member x l = union xs l
--             | otherwise = [x] ++ union xs l
               | otherwise = x:(union xs l)

-- intercecao
inter [] l = [] 
inter (x:xs) l | member x l = x:(inter xs l)
               | otherwise = inter xs l

-- diferenca
dif [] _ = [] -- o underscore ("_") é usado no lugar de uma variavel que não vai ser usada
dif l [] = l
dif (x:xs) l | member x l = dif xs l
             | otherwise = x:(dif xs l)

-- eliminar duplicatas de uma lista
eliminarDuplicata [] = []
eliminarDuplicata (x:xs) | member x xs = eliminarDuplicata xs
                         | otherwise = x:(eliminarDuplicata xs)

-- ############################################## --
-- ################### AULA 3 ################### --
-- ############################################## --mu6

creditar v (n,s) = (n,s+v)

cadastrar (n,s) l = l ++ [(n,s)]

procurar n [] = []
procurar n1 ((n2,s):l) | n1 == n2 = [(n2,s)]
                       | otherwise = (procurar n1 l)

creditarBanco n v [] = []
creditarBanco n1 v ((n2,s):l) | n1 == n2 = (n2,(s+v)):l
                              | otherwise = (n2,s):(creditarBanco n1 v l)

existe n l = (procurar n l) /= []

cadastrarVerificando c l = if (existe(fst c) l) then l else l ++ [c]

lista = [(1,100),(2,200),(3,300),(4,500),(5,500)]
ex = creditarBanco 1 50 lista

-- concatenacao de duas listas
--conca :: x -> y -> z
--conca :: [w] -> y -> y
--conca :: [w] -> y -> y   { x :: w , xs :: [w]}
--conca :: [w] -> [w] -> [w] -- por saber que ":" é sempre uma lista, então haskell sabre que o retorno é uma lista

conca [] b = b
conca (x:xs) b = x:(conca xs b)

-- reverte a ordem da lista
rev [] = []
rev (x:xs) = conca (rev xs) [x]

-- ############################################## --
-- ################### AULA 4 ################### --
-- ############################################## --

l1 = [1,2]
l2 = ["a","b"]
l3 = [1,2,3]

-- 
--zipi x -> y -> z 
--zipi [w] -> y -> [q] 
--zipi [w] -> [r] -> [q] {a :: w, as :: [w], b :: r, bs :: [r], (a,b) :: q}
--zipi [w] -> [r] -> [(w,r)]

-- zipi [] l = []
-- zipi l [] = []
zipi (a:as) (b:bs) = (a,b):(zip as bs)
zipi _ _ = []

contas = [(1,100),(2,200),(3,100)]

-- remove um elemento na posição n de um lista l
-- remover n [] = []
-- remover n1 ((n2,s):l) | n1 == n2 = l 
--                       | n1 /= n2 = (n2,s):(remover n1 l)

remover n l = [(n1,s) | (n1,s) <- l , n /= n1]

-- credita
creditar2 n v l = [(n1,s+v) | (n1,s) <- l , n == n1] 
                  ++ [(n1,s) | (n1,s) <- l , n /= n1]

-- quicksort
qsort [] = []
qsort (e:l) = qsort [x | x <- l, x < e]
              ++ [e]
              ++ qsort [x | x <- l, x >= e]


-- split divide uma lista em duas baseando em um pivor 
split p [] = ([],[])
split p (x:xs) | x < p = (x:a,b)
               | x >= p = (a,x:b)
               where (a,b) = split p xs


-- ############################################## --
-- ################### AULA 5 ################### --
-- ############################################## --

-- split 3 1:(4:(3:(2:[]))) = 
--               (1:e,d)      {p = 3, x = 1, xs = 4:(3:(2:[]))}  

-- split 3 [1,4,3,2]
-- split 3 [4,3,2]
-- split 3 [3,2] = 
-- split 3 [2] = (2:[],[])
-- split 3 [] = ([],[])

-- quicksort eficiente
qsortEf [] = []
qsortEf (x:xs) = qsortEf e ++ [x] ++ qsortEf d
                 where (e,d) = split x xs

-- pega o segundo elemento de uma lista
segundo (x:y:xs) = y

-- debitar
debitar n v l = [(n1,s-v) | (n1,s) <- l , n == n1] 
                ++ [(n1,s) | (n1,s) <- l , n /= n1]

-- sist
--sist [] estado = estado
--sist ((op,as):xs) estado | op == "cadastrar" = sist xs (cadastrar ((head as), 0) estado)
--                         | op == "creditar" = sist xs (creditar2 (head as) (read segundo as) estado)
--                         | op == "debitar" = sist xs (debitar (head as) (read segundo as) estado)

-- sistema de um banco que recebe uma lista de ações do banco
--sistema l = sist l []

-- est = sistema [superlista]

-- mxn 
mediaComBonus x y = if (x + y)/2 > 7 
                    then (x + y)/2 + 0.5
                    else (x + y)/2 

media x y = (x + y)/2

mediaComBonus2 x y = if media x y > 7
                     then (media x y) + 0.5
                     else (media x y) 

mediaComBonus3 x y = if m > 7
                     then m + 0.5
                     else m
                     where m = media x y 

mediaComBonus4 x y = m + (if m > 7 then 0.5 else 0)
                     where m = media x y 


-- ############################################## --
-- ################### AULA 6 ################### --
-- ############################################## --

-- aprovados 
aprovados [] = []
aprovados ((n,m):xs) | m >= 7 = (n,m):aprovados xs
                     | otherwise = aprovados xs

-- aprovados com compreenção de lista
aprovadosc l = [(n,m) | (n,m) <- l, m >= 7]

-- função map
mapr f [] = []
mapr f (e:l) = (f e) : mapr f l

-- mapr :: x -> y -> z
-- mapr :: x -> [w] -> [v]
-- mapr :: x -> [w] -> [v]              {e :: w, l :: [w]}
-- mapr :: (a -> b) -> [a] -> [b]       {e :: w/a, l :: [w]/[a]}

-- função map com compreenção de lista (ou conjuntos)
mapc f l = [f e | e <- l]

-- reprovados com compreenção de lista
reprovado l = [(n,m) | (n,m) <- l, m <= 7]

-- alunos basendo em um predicado p
alunos p [] = []
alunos p ((n,m):xs) | p m = (n,m):(alunos p xs)
                    | otherwise = alunos p xs
--
alunosl p l = [(n,m) | (n,m) <- l, p m]

menor7 m = m <= 7
maior7 m = m >= 7

aprovadosn l = alunos maior7 l
reprovadosn l = alunos menor7 l

--
mfilter p l = [e | e <- l, p e]

-- fold
-- fold (+) [1,3]
--      (+) 1 (fold (+) [3])
--      (+) 1 ((+) 3 (fold (+) []))
--      (+) 1 ((+) 3 (0))

foldq op [] = 0 
foldq op (x:xs) = op x (foldq op xs)

somaf l = fold (+) l
sizef l = fold soma1 l 

soma1 x y = 1 + y

-- fold padrão (i é a identidade, 0 no caso de somaf)
foldp op i [] = i 
foldp op i (x:xs) = op x (foldp op i xs)

-- ############################################## --
-- ################### AULA 7 ################### --
-- ############################################## --

-- tipo fold
fold op i [] = i
fold op i (x:xs) = op x (fold op i xs)

-- fold :: x -> y -> z -> w
-- fold :: x -> y -> [r] -> y
-- fold :: (r -> y -> y) -> y -> [r] -> y {r :: x, [r] :: [x], y :: i}

mediaaaux s t [] = s/t
mediaaaux s t (x:xs) = mediaaaux (s+x) (t+1) xs

mediafoud l = s/t
            where (s,t) = foldr (\x (a,b) -> (a+x,b+1)) (0,0) l

-- f.g x = f (g x)

-- ############################################## --
-- ################### AULA 8 ################### --
-- ############################################## --

-- toda função em haskell só tem um parâmetro
av1 (x,op,y) = av0op op x y

av0op '+' = (+)
av0op '-' = (-)
av0op '*' = (*)
av0op '/' = (/)

av1op '+' = "+"
av1op '-' = "-"
av1op '*' = "*"
av1op '/' = "/"

type Operador = Char
data Expressao = Numero Float
               | Unaria Operador Expressao
               | Binaria Expressao Operador Expressao

exp7 = Binaria (Numero 4) '+' (Binaria (Numero 6) '/' (Numero 2))

avaliard (Numero n) = n
avaliard (Unaria op exp) | op == '-' = -(avaliard exp)
                         | otherwise = avaliard exp
avaliard (Binaria exp1 op exp2) = av0op op (avaliard exp1) (avaliard exp2)

showExp (Numero n) = show n 
showExp (Unaria op exp) | op == '-' = "-" ++ showExp exp
                        | otherwise = show (showExp exp)
showExp (Binaria exp1 op exp2) = (showExp exp1) ++ av1op op ++ showExp (exp2)

data OpBinario = Soma | Sub | Mul | Div 
data OpUnaria = Soma | Sub
data Exp = Literal Float
         | Un OpUnaria Exp
         | Bin Exp OpBinario Exp

data Expp t = Litp t
            | Unp OpUnaria (Expp t)
            | Binp (Expp t) OpBinario (Expp t)

type ExpInt = Expp Int
type ExpStr = Expp String
type ExpFloat = Expp Float      1

eval lit (Litp l) = lit l 
eval lit (Unp op exp) | op == Pos = eval lit exp
                      | op == Neg = -(eval lit exp)
eval lit (Binp exp1 op exp2) = av0op op (eval lit exp1) (eval lit exp2)                      

-- ############################################## --
-- ################### AULA 9 ################### --
-- ############################################## --

-- "def inc = (lambda x . + x 1); 
--  def v = + 3 2;
--  def resultado = inc v"

data TermoLinFun = Literal Double
                 | Identifier String
                 | Lambda String TermoLinFun
                 | Aplicacao TermoLinFun TermoLinFun

-- Aplicacao = Operação, 
-- ela sempre será unária com ocorre com haskell,
-- onde uma função com dois termos será o retorno de uma função de um termo com o outro termo

data Definicao = Def String TermoLinFun
-- poderia ser também da seguinte forma: 
-- type Definicao = (String, TermoLinFun)

-- def inc = (lambda x . + x 1.0)
def1 = Def "inc" (Lambda "x" (Aplicacao (Aplicacao "+" (Identifier "x")) (Literal 1.0)))
-- def v = + 3.1 2.9
def2 = Def "v" (Aplicacao (Aplicacao (Identifier "+") (Literal 3.1)) (Literal 2.9))
-- def resultado = inc c
def3 = Def "resultado" (Aplicacao (Identifier "inc") (Identifier "v"))
prog1 = [def1, def2, def3]

type Programa = (Definicao)

--Saida deo Interpretador

data ValorFun = Numero Double
              | Funcao (ValorFun -> ValorFun)
              | Excecao

instance Show ValorFun where
  show (Numero n) = show n
  show (Funcao f) = "Function definition cannot be printed"
  show Excacao = "Execao durante execucao do interpretador"

-- 

type Ambiente = [(Id,ValorFun)]

ambientesimples = [("+",Funcao (\x -> (Funcao (\y -> somaValorFun x y))))]

somaValorFun (Numero x) (Numero y) = Numero (x+y)
somaValorFun _ _ = Excecao

intTermo a (Literal n) = Numero n
intTermo a (Identifier i) = getValor i a
intTermo a (Lambda i t) = Funcao (\x -> intTermo (i,x):a t)
intTermo a (Aplicacao t1 t2) = aplica v1 v2 
                             where v1 = intTermo a t1
                                   v2 = intTermo a t2

intPrograma a [] = Execacao
intPrograma a [Def i t] = intTermo a t
intPrograma a [(Def i t):ds] = intPrograma ((i,intTermo a t):a) ds


getValor i [] = Execao
getValor i ((j,v):l) = if i == j
                       then v
                       else getValor i l 

aplica (Funcao f) v = f v
aplica _ _ = Excecao

















