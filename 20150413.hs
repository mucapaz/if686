-- trabalho 7

comp :: (t -> u) -> (u -> v) -> (t -> v)
comp g f =  fog
	where fog x = f (g x)
	
compose :: (u->v) -> [(t->u)] -> [(t->v)]	
compose f [] = []
compose f (g:as) = [comp g f] ++ compose f as

-- head (compose (+1) [(+3)]) 1
