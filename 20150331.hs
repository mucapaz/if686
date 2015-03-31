-- Aqui começa o trabalho 04


--q1

{-
1ª)
O polimorfismo de haskell é mais seguro quanto a garantia de
que os tipos terão implementações das funcionalidades que serão
utilizadas sob eles, pois é possível restringir que os argumentos genéricos
possuam implementações de determinadas funções, sendo esses definidos por uma
classe. Em Java, o Generics que é utilizado no polimorfismo acaba sendo mais
geral pois não restringe os dados, porém é mais sucetível à erro tendo em vista que
apenas ocorre uma verificação de tipo em tempo de execução e depois ocorre um "type erasure"
para gerar compatibilidade com as JVM antigas, pois Generics foi adicionada a partir do Java 5.
-}

--q2

ge :: String -> Char -> Int -> String
ge [] b c =  (show c) ++ [b] 
ge a b c 
	| head a == b = ge(tail a) (b) (c+1) 
	| head a /= b = (show c) ++ [b] ++ ge (a) (head a) (0)
	
nl :: String -> String
nl a = ge (a) (head a) (0)

ls :: Int -> String
ls 0 = "1"
ls a = nl (ls (a-1)) 

lookAndSay :: Enum j => j -> String 
lookAndSay a = ls (fromEnum(a))

--q3
type Rotulo  = String
type Grafo = [(Rotulo, Rotulo)]

base :: Grafo
base = [("3","2"),("2","9"),("9","1"),("1","2"),("10","8"),("4","2"),("4","10"),("10","8"),("2","4")]

retira :: Grafo -> Rotulo -> Grafo
retira [] a = []
retira ((a,b):c) x
	| a == x = c
	| a /= x = [(a,b)] ++ (retira c x)

proximo :: Grafo -> Rotulo -> Rotulo
proximo ((a,b):c) x
	| a == x = b
	| otherwise = proximo c x

se :: Grafo -> Rotulo -> Rotulo -> ([Rotulo], Bool)
se as a b
	| a == b = ([b],True)
	| as == [] = ([],False)
	| (retira as a) == as = ([],False) -- dado que a != b e as != [] verifico se tem opção para andar 
	| snd(se (retira as a) (proximo as a) (b)) == True = ([a] ++ fst(se (retira as a) (proximo as a) b),True) 
	| otherwise = (fst( se (retira as a) (a) (b)), snd( se (retira as a) (a) (b)))

search :: Grafo -> Rotulo -> Rotulo -> [Rotulo]
search as a b
	| snd (se as a b) == True = fst (se as a b)
	| otherwise = []  

--q4

size :: [i] -> Int
size [] = 0
size as = (size (tail as)) + 1

buscaAux :: [Int] -> Int -> Int -> [Int]
buscaAux [] a b = []
buscaAux as a b
	| a == b && a == 0 = [head as]
	| a == 0 = [head as]++ (buscaAux (tail as) 0 (b-1))
	| otherwise = (buscaAux (tail as) (a-1) (b-1))

busca :: [[Int]] -> Int -> Int -> Int -> Int-> [Int]
busca [] nx ny mx my = []
busca as nx ny mx my
	| nx == mx && nx == 0 = (buscaAux(head as) ny my)
	| nx == 0 = (buscaAux(head as) ny my) ++ (busca (tail as) nx ny (mx-1) my)
	| otherwise = (busca (tail as) (nx-1) (ny) (mx-1) my)

quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort as = quicksort[a| a <- (tail as), a < (head as)] ++ [head as] ++  quicksort[a| a <- (tail as), a >= (head as)]
	
medAuxPar :: [Int] -> Int -> Int
medAuxPar as a
	| a == 0 = (div (head as)  2) + medAuxPar (tail as) (a-1) 
	| a == -1 =  (div (head as) 2) 
	| otherwise = medAuxPar (tail as) (a-1)

medAuxImpar :: [Int] -> Int -> Int
medAuxImpar as a
	| a == 0 =  head as 
	| otherwise = medAuxImpar (tail as) (a-1)
	
med :: [Int] -> Int
med as 
	| (mod (size as) 2) == 0 = medAuxPar as (div (size as) 2) 
	| otherwise = medAuxImpar as (div (size as) 2)
	
medianaAux :: [[Int]] -> Int -> Int -> Int -> Int -> Int
medianaAux base nx ny mx my = med(quicksort((busca base nx ny mx my)))

mediana :: [[Int]] -> Int -> Int -> Int-> Int-> Int -> Int -> [Int]
mediana base nx ny mx my maxx maxy 
	| nx < 0 || ny < 0 || mx >= maxx || my >= maxy = [0]
	| otherwise = [medianaAux base nx ny mx my]
	
-- x - n > 0 && x + n < mx && y - n > 0 && y + n < my
fm :: [[Int]] -> [Int] -> Int -> Int -> Int -> Int-> [[Int]]
fm base aux n mx my i
	| i == (mx * my) = []
	| y == my - 1 = [m] ++ fm (base) [] n mx my (i+1)   
 	| otherwise =  fm (base) (m) n mx my (i+1)   
	
	where x = div i my;
		y = i - x*my;
		m = aux ++ (mediana (base) (x-n) (y-n) (x+n) (y+n) mx my );

filtroMediana :: [[Int]] -> Int -> [[Int]]
filtroMediana base n
	| size base == 0 = [[]] 
	| otherwise = fm (base) [] (div n 2) (size base) (size (head base)) 0

base4 :: [[Int]]
base4 = [[9,4,5,0,8],[10,3,2,1,7],[9,1,6,3,15],[0,3,8,10,1],[1,16,9,12,7]]




