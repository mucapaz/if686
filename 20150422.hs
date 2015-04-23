-- Aqui começa o trabalho 8
data No t = Node t [(t,Int)] deriving(Eq,Show)
data Graph t = Grafo [No t] deriving(Eq,Show)

getNodePosition :: Eq t => [No t] -> t -> Int 
getNodePosition ((Node a ai):as) n
	| a == n = 0
	| otherwise = (getNodePosition as n) + 1

getNodeAux :: Eq t=> [No t] -> Int -> No t
getNodeAux (a:as) 0 = a
getNodeAux (a:as) n = getNodeAux as (n-1) 	

getNode :: Eq t=> [No t] -> t -> No t
getNode as t = getNodeAux as (getNodePosition as t)

hasNextNode :: Eq t => [No t] -> t -> Bool
hasNextNode as t = not(nr == []) where (Node n nr) = getNode as t  


getNextNode :: Eq t => [No t] -> t -> t
getNextNode as t = fst(head nr) where (Node n nr ) = getNode as t

getNextWeight :: Eq t => [No t] -> t -> Int
getNextWeight as t = snd(head nr) where (Node n nr ) = getNode as t

getNextGraphAux :: Eq t => [No t] -> Int -> [No t]
getNextGraphAux ((Node a (b:bs)):as) 0 = [Node a bs] ++ as
getNextGraphAux (a:as) n = [a] ++ getNextGraphAux as (n-1)
 

getNextGraph :: Eq t => [No t] -> t -> [No t]
getNextGraph as t = getNextGraphAux as (getNodePosition as t)  


search :: Eq t => [No t] -> t -> t -> Int
search as at fim
	| at == fim = 0
	| hasNextNode as at == False = 100000000
	| otherwise = min (search (getNextGraph as at) at fim) ((search (getNextGraph as at) (getNextNode as at) fim) + (getNextWeight as at)  )

geraFuncaoMenorCaminho :: Eq t => [No t] -> (t -> t -> Int)
geraFuncaoMenorCaminho as = (\x y -> (search as x y))

{-
 (search (getNextGraph as at) (getNextNode as at)) + (getNextWeight as at) 
nextNo :: [No t]-> t -> t
nextNo ((Node i ((a,b):ai)):as) = i
-}

