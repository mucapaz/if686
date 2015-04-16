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

f :: Int -> Int -> Int
f a b = a + b

g a b = (\b a -> f b a)

pri :: [(t,u)] -> [t]
pri [] = []
pri ((a,b):as) = [a] ++ pri as 

pri1 :: [(t,u)] -> [t]
pri1 as =  [fi a | a <- as] 
	where fi x = fst x
	
pri2 :: [(t,u)] -> [t]
pri2 as =  [ (\x -> fst x) a | a <- as] 
	
seg1 :: [[Int]] -> Int -> [[Int]]
seg1 as n= [a| a<-as, (\x -> (length x) > n) a]

{-
ter1Aux :: [Int] -> [Bool]
ter1Aux (a:as) = (\x -> x elem as) as 
-}


rdup :: [Int] -> [Int]
rdup [] = []
rdup (a:as)
	| elem a as = rdup as
	| otherwise = [a] ++ rdup as
	
ter1 :: [[Int]] -> [Int]
ter1 as = [b | a<-as, b<-a] 

{-
sumxAux :: Int -> [Int] -> [Int]
sumxAux x [] = []
sumxAux x (a:as) = [x + a] ++ sumxAux x as   

sumx :: [Int] -> Int -> [Int]
sumx as x = (sumxAux x) as
-}

sx :: [Int] -> Int -> [Int]
sx as x = map (+x) as

maux :: Int -> Int -> Int
maux a b
	| a > b = a
	|otherwise = b

--mx :: [Int] -> Int
-- mx as = 


-- data Tree t = Nil t | Node t (Tree t) (Tree t)

-- ter1 as = [ [ (\x -> ) |a<-as ] ]
	
{-
pri2 :: [(t,u)] -> [t]
pri2 as = (\as -> )


(\as ) ->  

ma :: (Ord t) => [t] -> t -> [t]
ma [] x = []
ma (a:as) x
	| 
	
-}
