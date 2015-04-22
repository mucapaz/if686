-- trabalho 8
qsort :: Ord t => [t] -> [t]
qsort [] = []
qsort (a:as) = (qsort [x| x<-as,x<=a]) ++ [a] ++ (qsort [x| x<-as,x>a])

gen :: (Ord t, Num t) => t => [t] => [t]
gen x [] = []
gen x (a:as)
	| a <= x = [a] ++ gen x as 	
	| otherwise = gen x as

nxt :: (Ord t, Num t) => t => [t] => [t]
nxt x [] = []
nxt x (a:as)
	| a > x = [a] ++ nxt x as 	
	| otherwise = nxt x as
	
laux :: (Ord t, Num t) => [t] -> [t] -> [[t]]
laux [] b = [b]
laux a [] = []
laux (a1:a2) b = [gen a1 b] ++ laux a2 (nxt a1 b)    

lpat :: (Ord t,Num t) => [t] -> [t] -> [[t]]
lpat a b = laux (qsort a) (qsort b)	

listPartitioner :: (Ord t,Num t) => [t] -> ([t] -> [[t]])
listPartitioner as = lpat as

-- exercicios em aula

{-
funcao :: Int -> Int -> Int
funcao a b = a + b

f :: [Int] -> Int
f a = ((map.foldr) funcao a) a
-}
----
--e1
sc :: Int -> ([Int]->[Int])
sc c = (\x -> map (+c) x)  
--e2
li :: ([Int] -> Int)
li = (\x -> foldr max (head x) x)
--e3
data Tree t = Nil| Node t (Tree t) (Tree t)

lis :: Tree t -> Tree t -> Bool
lis Nil Nil = True
lis Nil (Node a b c) = False
lis (Node a b c) Nil = False
lis (Node a1 b1 c1) (Node a2 b2 c2) = (lis b1 b2) && (lis c1 c2) || (lis b1 c2) && (lis c2 b1) 

iso :: Tree t -> (Tree t -> Bool)
iso a = (lis a)  	
--e5

uni :: [t] -> [t] -> [(t,t)]
uni [] [] = []
uni [] (a:as) = [(a,a)] ++ uni [] as
uni (a:as) [] = [(a,a)] ++ uni as []
uni (a:as) (b:bs) = [(a,b)] ++ uni as bs

lo :: [t] -> ([t]->[(t,t)])
lo a = (\x -> (uni a) x)

