-- Aqui começa o código do trabalho 
-- Questao 1 do trabalho
import Data.Char

type Cpf = String
type Nome = String
type Pessoa = (Cpf,Nome)
type Hash = [[Pessoa]]

base :: Hash
base = [[("000.000.000-00","Samuel"),("000.000.000-55","Rene")],[("000.000.000-01","Rena")],[],[],[],[],[],[],[],[]]

evaluate :: Cpf -> Int
evaluate as
	| as == [] = 0
	| isDigit (head as) = mod ( digitToInt (head as) + (evaluate (tail as)) ) (10)
	| otherwise = mod (evaluate (tail as)) (10) 

getByCpf :: [Pessoa] -> Cpf -> Pessoa
getByCpf [] c = ("","")
getByCpf ((x,y):z) c
	| x == c = (x,y)
	| otherwise = getByCpf z c

getByEvaluate :: Hash -> Cpf -> Int -> Pessoa 
getByEvaluate h c 0 = getByCpf (head h) (c)  
getByEvaluate h c i = getByEvaluate (tail h) (c) (i-1)

get :: Hash -> Cpf -> Pessoa
get h c = getByEvaluate (h) (c) (evaluate c)

putByCpf :: [Pessoa] -> Pessoa -> [Pessoa]
putByCpf [] p = [p]
putByCpf as p = [head as] ++ putByCpf (tail as) p

putByEvaluate :: Hash -> Pessoa -> Int -> Hash
putByEvaluate h p 0 = [(putByCpf (head h) p)] ++ (tail h) 
putByEvaluate h p i = [head h] ++ (putByEvaluate (tail h) p (i-1)) 

put :: Hash -> Pessoa -> Hash
put h (a,b) = putByEvaluate (h) (a,b) (evaluate a)

removeByCpf :: [Pessoa] -> Cpf -> [Pessoa]
removeByCpf [] c = []
removeByCpf ((x,y):z) c
	| x == c = z
	| otherwise = [(x,y)] ++ removeByCpf z c

removeByEvaluate :: Hash -> Cpf -> Int -> Hash
removeByEvaluate h c 0 = [removeByCpf (head h) c] ++ (tail h)
removeByEvaluate h c i = [head h] ++ (removeByEvaluate (tail h) c (i-1))

remove :: Hash -> Cpf -> Hash
remove h c = removeByEvaluate h c (evaluate c) 


hasKeyByCpf :: [Pessoa] -> Cpf -> Bool
hasKeyByCpf [] c = False
hasKeyByCpf ((x,y):z) c
	| x == c = True
	| otherwise = hasKeyByCpf z c

hasKeyByEvaluate :: Hash -> Cpf -> Int -> Bool
hasKeyByEvaluate h c 0 = hasKeyByCpf (head h) (c)
hasKeyByEvaluate h c i = hasKeyByEvaluate (tail h) (c) (i-1)

hasKey :: Hash -> Cpf -> Bool
hasKey h c = hasKeyByEvaluate h c (evaluate c) 

-- Questao 2 do trabalho

uniqueAux :: [Int] -> Int -> [Int]
uniqueAux [] i = []
uniqueAux as i
	| head as == i = (uniqueAux (tail as) i)
	| otherwise = [head as] ++ (uniqueAux (tail as) i)

unique :: [Int] -> [Int]
unique [] = []
unique as = [head as] ++ (unique (uniqueAux (tail as) (head as)))

countInterAux :: [Int] -> Int -> Int
countInterAux [] i = 0
countInterAux (a:b) i
	| a == i = (countInterAux b i) + 1
	| otherwise = (countInterAux b i) 

countInter :: [Int] -> [Int] -> Int
countInter [] c = 0
countInter (a:b) c = (countInter b c) + (countInterAux c a) 

size :: [Int] -> Int
size [] = 0
size (a:b) = (size b) + 1

comparaConjuntosUnique :: [Int] -> [Int] -> String
comparaConjuntosUnique a b
	| ((countInter a b) == (size a)) && ((size a) == (size b)) = "A igual a B"
	| (countInter a b) == (size a) = "B contem A"
	| (countInter a b) == (size b) = "A contem B"
	| ((countInter a b) > 0) = "A interseciona B" 
 	| ((countInter a b) == 0) = "Conjuntos dijuntos"
 	
comparaConjuntos :: [Int] -> [Int] -> String
comparaConjuntos a b = comparaConjuntosUnique (unique a) (unique b)




















