-----------------AQUI COMEÇA O CÓDIGO DO TRABALHO -------------------------------

------------------MERGESORT----------------------------------------
tamanho :: [Int] -> Int
tamanho [] = 0
tamanho (a:as) = 1 + tamanho as

getByIndex :: [Int]-> Int -> [Int]
getByIndex as x
		| x == 0 = [head as]
		| otherwise = getByIndex (tail as) (x-1)

merge :: [Int]-> [Int] -> [Int]
merge as [] = as
merge [] as = as
merge as bs
	| head as < head bs = [head as] ++ merge (tail as) (bs)
	| otherwise = [head bs] ++ merge (as) (tail bs) 

msort :: [Int] -> Int -> Int-> [Int]
msort as i j
	| i == j = getByIndex as i
	| otherwise = merge (msort as i (div (i+j) 2)) (msort as ((div (i+j) 2)+1) j)
	
mergesort :: [Int] -> [Int]
mergesort as = msort (as) (0) (tamanho (as) -1)

------------------HEAPSORT-----------------------------------------

getElementByIndex :: [Int] -> Int -> Int
getElementByIndex as x
	| x == 0 = head as
	| otherwise = getElementByIndex (tail as) (x-1)

changeAux :: [Int] -> Int -> Int -> [Int]
changeAux as i j
	| i == 0 = [j] ++ tail as
	| otherwise = [head as] ++ changeAux (tail as) (i-1) (j) 
	
change :: [Int] -> Int -> Int-> [Int]
change as i j
	| True = changeAux (changeAux (as) (i) (getElementByIndex as j) ) (j) (getElementByIndex as i) 

le :: Int -> Int
le a = a + a

ri :: Int -> Int
ri a = a + a + 1

validIndex :: Int -> Int -> Bool
validIndex i n
	| i >= 1 && i <= n = True
	|otherwise = False

maxHeap :: [Int] -> Int -> Int -> [Int]
maxHeap as i n
	---- right i é o maior que left i e i
	|(validIndex (le i) n) && (validIndex (ri i) n) && (getElementByIndex as (ri i)) > (getElementByIndex as i) && (getElementByIndex as (ri i)) >= (getElementByIndex as (le i)) = (maxHeap (change as (ri i) i) (ri i) (n))
	--- left i é maior que i  e right i 
	|(validIndex (le i) n) && (validIndex (ri i) n) && (getElementByIndex as (le i)) > (getElementByIndex as i) && (getElementByIndex as (le i)) >= (getElementByIndex as (ri i)) = maxHeap (change as (le i) i) (le i) n 
	| (validIndex (le i) n) && (getElementByIndex as (le i)) > (getElementByIndex as i) = (maxHeap (change as (le i) i) (le i) n)
	| otherwise = as 

lacoHeap :: [Int] -> Int -> Int -> [Int]
lacoHeap as i n
	| i >= 1 = lacoHeap (maxHeap as i n) (i-1) (n)  
	| otherwise = as	
	
buildMaxHeap :: [Int] -> Int -> [Int]
buildMaxHeap as n 
	| True = lacoHeap as (div n 2) n

lacoHeapsort :: [Int] -> Int -> Int-> [Int]
lacoHeapsort as i m
	| i >= 2 = lacoHeapsort (maxHeap (change as 1 i) (1) (m-1)) (i-1) (m-1)
	| otherwise = as

heapsort :: [Int] -> [Int]
heapsort as
	| True = lacoHeapsort (buildMaxHeap as ((tamanho as)-1)) ((tamanho as)-1) ((tamanho as)-1)

----------------------------- AQUI COMECA O EXERCICIO DA AULA --------------------------------------

---------------exercicio slide 6---------------------
menor :: Int -> Int -> Int
menor a b
	| a<b = a
	|otherwise = b

maior :: Int -> Int -> Int
maior a b
	| a>b = a
	| otherwise = b

meio :: Int -> Int -> Int -> Int
meio a b c
	| a>= b && a<=c = a
	| a>= c && a<=b = a
	| b>= a && b<=c = b
	| b>= c && b<=a = b
	| c>= b && c<=a = c
	| c>= a && c<=b = c
	
menorMaior :: Int -> Int -> Int -> (Int, Int)
menorMaior a b c
	| True = ((menor (menor a b) c),( maior (maior a b) c ))

ordenaTripla :: (Int,Int,Int) -> (Int, Int, Int)
ordenaTripla (a,b,c) 
	| True = ((menor (menor a b) c), meio a b c , (maior (maior a b) c)) 


-----------exercicio slide  7 e 8-----------------------

type Ponto = (Float,Float)
type Reta = (Ponto,Ponto)

primeiraCoordenadaPonto :: Ponto -> Float
primeiraCoordenadaPonto (x,y) = x

segundaCoordenadaPonto :: Ponto -> Float
segundaCoordenadaPonto (x,y) = y

retaVertical :: Reta -> Bool
retaVertical ((x1,y1),(x2,y2)) = (x1 == x2) 

coordY :: Reta -> Float -> Float
coordY ((x1,y1),(x2,y2)) x
	| retaVertical ((x1,y1),(x2,y2)) = 0
	| otherwise = (((y2 - y1)/(x2 - x1)) * (x - x1)) + y1


-- Exercicio slide  13 ao 16

type Pessoa = String
type Livro = String
type BancoDados = [(Pessoa,Livro)]

baseExemplo :: BancoDados
baseExemplo =  [("Sergio","O Senhor dos Aneis"),("Andre","sDuna"), ("Fernando","Jonathan Strange & Mr. Norrell"),  ("Fernando","A Game of Thrones")]

livros :: BancoDados-> Pessoa -> [Livro]
livros [] p = []
livros ((a,b):c) p 
	| a == p = [b] ++ livros c p
	| otherwise = livros c p

emprestimos :: BancoDados -> Livro -> [Pessoa]
emprestimos [] l = []
emprestimos ((a,b):c) l
	| b == l = [a] ++ (livros c l)
	| otherwise = (livros c l)

emprestado :: BancoDados -> Livro -> Bool
emprestado [] l = False
emprestado ((a,b):c) l
	| b == l = True
	| otherwise = emprestado c l
	
qtdEmprestimos :: BancoDados -> Pessoa -> Int
qtdEmprestimos [] p = 0
qtdEmprestimos ((a,b):c) p
	| a == p = (qtdEmprestimos c p)+1
	| otherwise = qtdEmprestimos c p

devolver :: BancoDados -> Pessoa -> Livro -> BancoDados
devolver [] p l = []	
devolver ((a,b):c) p l
	| p == a && l == b = c
	| otherwise = (a,b) : (devolver c p l)
