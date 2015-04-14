-- trabalho 7

comp :: (t -> u) -> (u -> v) -> (t -> v)
comp g f =  fog
	where fog x = f (g x)
	
compose :: (u->v) -> [(t->u)] -> [(t->v)]	
compose f [] = []
compose f (g:as) = [comp g f] ++ compose f as

-- head (compose (+1) [(+3)]) 1

data Node t = No t [t] deriving (Eq, Show)
data Graph t = Grafo [Node t] deriving (Eq, Show)

mapNode :: (t->v) -> Node t -> Node v 
mapNode f (No a as) = No (f a)  (map f as)  

mapGraphAux :: (t->v) -> [Node t] -> [Node v]
mapGraphAux f [] = []
mapGraphAux f (a:as) = [mapNode f a] ++ mapGraphAux f as

mapGraph :: (t->v) -> Graph t -> Graph v
mapGraph f (Grafo  as) = (Grafo (mapGraphAux f as)) 

baseGraph :: Num t => Graph t 
baseGraph = (Grafo [(No 1 [2]), (No 10 [12])])

foldGraphAux :: (t->t->t) -> [t] -> t
foldGraphAux f [a] = a 
foldGraphAux f (a:as) = f a (foldGraphAux f as)

foldGraph :: Graph t -> (Node t->Node t->Node t) -> Node t 
foldGraph (Grafo as) f = foldGraphAux f as  


data Tree t = Nil | Node t (Tree t) (Tree t) deriving (Show,Eq)

filterAux :: (t->Bool) -> Tree t -> Tree t
filterAux f Nil = Nil
filterAux f (Node a b c)
	| f a = Node a (filterAux f b) (filterAux f c) 
	| otherwise = Nil

next :: (t->Bool) -> Tree t -> [Tree t]
next f Nil = [Nil]
next f (Node a b c)
	| f a == False = [b] ++ [c]
	| otherwise = (next f b) ++ (next f c)

filterLoop :: (t->Bool )-> [Tree t] -> [Tree t] 
filterLoop f [] = []
filterLoop f (a:as) = (filterTree f a) ++ (filterLoop f as)

	
filterTree :: (t->Bool) -> Tree t -> [Tree t]
filterTree f Nil = []
filterTree f (Node a b c)
	| f a = [filterAux f (Node a b c)] ++ (filterLoop f (next f (Node a b c)) )
	| otherwise = (filterTree f b) ++  (filterTree f c)


-- filterTree (<10) (Node 5 (Node 7 (Node 15 Nil (Node 6 Nil Nil)) (Node 2 Nil Nil)) (Node 10 Nil Nil))




























