import Data.Char
-- Aqui comeca o codigo do trabalho 6 

data Node t = No t [t] deriving (Eq, Show)
data Graph t = Grafo [Node t] deriving (Eq, Show)

getNodePosition :: Eq t => [Node t] -> t -> Int -> Int
getNodePosition ((No a b): c) n i  
	| a == n = i 
	| otherwise = getNodePosition c n (i+1)
 
getNode :: Eq t => [Node t] -> Int -> Int -> t
getNode ((No a b):c) i j
	| i == j = a
	| otherwise = getNode c (i+1) j  

getAdj :: Eq t => [Node t] -> Int -> Int -> [t]
getAdj ((No a b):c) i j
	| i == j = b
	| otherwise = getAdj c (i+1) j  

nextGraph :: Eq t => [Node t] -> Int -> Int -> [Node t]
nextGraph ((No a b):c) i j
	| i == j = [No (a) (tail b)] ++ c      
    | otherwise = [No (a) (b)] ++ (nextGraph c (i+1) j) 
 
   
    
dfsAux :: Eq t => [Node t] -> Int -> t -> Bool
dfsAux as i value 
	| (getNode as 0 i) == value = True
	| (getAdj as 0 i) == [] = False
	| dfsAux (nextGraph as 0 i) ( getNodePosition (as) (head (getAdj as 0 i) ) (0)) value == True = True
	| otherwise = dfsAux (nextGraph as 0 i) i value


dfsLoop :: Eq t => Graph t -> Int -> Int -> t -> Bool
dfsLoop (Grafo as) i j value
	| i == j = False
	| dfsAux as i value = True
	| otherwise = dfsLoop (Grafo as) (i+1) (j) value 


dfs :: Eq t => Graph t -> t -> Bool
dfs (Grafo gr) value = dfsLoop (Grafo gr) 0 (length gr) value


-- Aqui comeca o codigo dos exercicios em aula

sqr :: Int -> Int
sqr n = n * n

quad :: [Float] -> [Float]
quad as = map sqrt as 

posicao :: Char -> Int
posicao a = ((ord a) - (ord 'a') + 1)

posicaoAlfabeto :: String -> [Int]
posicaoAlfabeto str = map posicao str

mapi :: (t->u) -> [t] -> [u]
mapi f as = [f a| a <- as]

fold :: (t->t->t) -> [t] -> t
fold f [a] = a 
fold f (a:as) = f a (fold f as)

andi :: [Bool] -> Bool
andi as = fold (&&) as	
{-
foldri :: (t->u->u) -> u -> [t] -> u  
foldri f s [] = s
foldri f s (a:as) = f a (foldri f s as) 
-}
-- memberAux :: 


member :: Eq t => t -> [t] -> Bool
member a as = foldr (||) (False) (map (==a) as)

f :: Eq t => t -> [t] -> [t]
f a b
	| elem a b = b
	| otherwise = [a] ++ b
			
union :: Eq t => [t] -> [t] -> [t]
union a b = foldr f [] (a++b) 

-- vemMonstro :: String -> [Int] -> [Int]
-- vemMonstro str as = [(fstrAux str)] ++ as


{-
flist :: [String] -> [Int]
flist [] = []
flist (a:as) = [(fstrAux a)] ++ (flist as)

-}

faux :: String -> [Int]
faux str = [fstrAux str]

fstrAux :: String -> Int
fstrAux [] =  0
fstrAux (a:as) = (ord a - (ord 'a') + 1)  + fstrAux as 


fstr :: [String] -> [Int]
fstr as = foldr (++) [] (map faux as)  

{-

-}

--map fstrAux as 
















