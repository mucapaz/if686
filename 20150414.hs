-- trabalho 7

comp :: (t -> u) -> (u -> v) -> (t -> v)
comp g f =  fog
	where fog x = f (g x)
	
compose :: (u->v) -> [(t->u)] -> [(t->v)]	
compose f [] = []
compose f (g:as) = [comp g f] ++ compose f as

-- head (compose (+1) [(+3)]) 1


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


