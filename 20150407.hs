data Dias = Segunda Int [String]| Terca Int [String]| Quarta Int [String] | Quinta Int [String] | Sexta Int [String] | Sabado  | Domingo

verifica :: Dias -> Bool
verifica ( Sabado ) = False
verifica ( Domingo ) = False
verifica ( _ ) = True

plcAux :: [String] -> Bool
plcAux [] = False
plcAux a
	| head a == "PLC" = True
	| otherwise = plcAux (tail a)
	
plc :: Dias -> Bool
plc (Segunda a b) = plcAux b
plc (Terca a b) = plcAux b
plc (Quarta a b) = plcAux b
plc (Quinta a b) = plcAux b
plc (Sexta a b) = plcAux b
plc (Sabado) = False
plc (Domingo) = False

data Shape = Circle Float | Rectangle Float Float
area :: Shape -> Float
area (Circle a) = (3.14)*(a*a)
area (Rectangle a b) = (a*b)

data Expr = Lit Int | Add Expr Expr | Sub Expr Expr deriving (Show)
data Tree t = Nil t| Node t (Tree t) (Tree t) deriving (Eq,Show) 
data List t = Null | Cons t (List t) deriving (Show)

showExpr :: Expr -> String
showExpr (Lit t) = show(t)
showExpr (Add a b) = "(" ++ (showExpr a)++ " + " ++ (showExpr b) ++ ")"
showExpr (Sub a b) = "(" ++ (showExpr a)++ " - " ++ (showExpr b) ++ ")"

toList :: List t -> [t]
toList (Null ) = []
toList (Cons a b) = [a] ++ toList b

fromList :: [t] -> List t
fromList [] = (Null)
fromList (a:as) = (Cons a (fromList as)) 

depth :: Tree t -> Int
depth (Nil a) = 0
depth (Node a b c) = (max (depth b) (depth c)) + 1

collapse :: Tree t -> [t]
collapse (Nil a) = [a]
collapse (Node a b c) = [a] ++ collapse b ++ collapse c

getValues :: [Tree t] -> [t]
getValues [] = []
getValues ((Nil a):as) = [a] ++ getValues as
getValues ((Node a b c):as) = [a] ++ getValues as 

getSons :: [Tree t] -> [Tree t]
getSons [] = []
getSons ((Nil a):as) = [] ++ (getSons as)
getSons ((Node a b c):as) = [b] ++ [c] ++ (getSons as)


bfsAux :: Eq t => [Tree t] -> t -> Bool
bfsAux [] x = False
bfsAux as x
	| (elem x (getValues as)) = True
	| otherwise = bfsAux (getSons as) x

bfs :: Eq t => Tree t -> t -> Bool
bfs as x = (bfsAux [as] x) 

-- bfs (Node 1 (Node 2 (Nil 3) (Nil 4)) (Node 5 (Node 6 (Nil 7) (Nil 8)) (Nil 9))) 7

toStr :: (Show t) => t -> String
toStr a = "!" ++ show a ++ "!"

mapTree :: (t->u) -> Tree t -> Tree u
mapTree func (Nil a) = (Nil (func a))
mapTree func (Node a b c) = (Node (func a) (mapTree func b) (mapTree func c))





