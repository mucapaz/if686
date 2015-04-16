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
