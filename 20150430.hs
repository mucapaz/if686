-- Exercícios em casa
data Failable t = Correct t | Error String deriving (Show)

instance Monad Failable where
	(>>=) (Error s) f = Error s
	(>>=) (Correct x) f = f x
	return x = Correct x

data Fila t = Fi Int [t] deriving (Show)

criarFila :: Int -> t -> Failable (t, Fila t)
criarFila 0 x = Error "Tam < 1"
criarFila i x = Correct(x, Fi i [x])

push :: t -> Fila t -> Failable (t, Fila t)
push (x) (Fi i (a:as))
	| i < length (a:as) = Error "Erro no push"
	| otherwise = Correct (x, Fi i ((a:as) ++ [x]) )

pop :: Fila t -> Failable (t, Fila t)
pop (Fi i (a:as))
	| length (a:as) == 0 = Error "Não da para tirar mais"
	| otherwise = Correct (a, Fi i as)

peek :: Fila t -> Failable (t, Fila t)
peek (Fi i (a:as))
	| length (a:as) == 0 = Error "Não da para tirar mais"
	| otherwise = Correct (a, Fi i (a:as) )
