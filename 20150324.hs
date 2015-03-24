------------------MERGESORT----------------------------------------
tamanho :: [Int] -> Int
tamanho [] = 0
tamanho (a:as) = 1 + tamanho as

getByIndex :: [Int]-> Int -> [Int]
getByIndex as x
		| x == 0 = [head as]
		| otherwise = getByIndex (tail as) (x-1)

merge :: [Int]-> [Int] -> [Int]
merge as [] = as
merge [] as = as
merge as bs
	| head as < head bs = [head as] ++ merge (tail as) (bs)
	| otherwise = [head bs] ++ merge (as) (tail bs) 

msort :: [Int] -> Int -> Int-> [Int]
msort as i j
	| i == j = getByIndex as i
	| otherwise = merge (msort as i (div (i+j) 2)) (msort as ((div (i+j) 2)+1) j)
	
mergesort :: [Int] -> [Int]
mergesort as = msort (as) (0) (tamanho (as) -1)

------------------HEAPSORT-----------------------------------------

getElementByIndex :: [Int] -> Int -> Int
getElementByIndex as x
	| x == 0 = head as
	| otherwise = getElementByIndex (tail as) (x-1)

changeAux :: [Int] -> Int -> Int -> [Int]
changeAux as i j
	| i == 0 = [j] ++ tail as
	| otherwise = [head as] ++ changeAux (tail as) (i-1) (j) 
	
change :: [Int] -> Int -> Int-> [Int]
change as i j
	| True = changeAux (changeAux (as) (i) (getElementByIndex as j) ) (j) (getElementByIndex as i) 

le :: Int -> Int
le a = a + a

ri :: Int -> Int
ri a = a + a + 1

validIndex :: Int -> Int -> Bool
validIndex i n
	| i >= 1 && i <= n = True
	|otherwise = False

maxHeap :: [Int] -> Int -> Int -> [Int]
maxHeap as i n
	---- right i é o maior que left i e i
	|(validIndex (le i) n) && (validIndex (ri i) n) && (getElementByIndex as (ri i)) > (getElementByIndex as i) && (getElementByIndex as (ri i)) >= (getElementByIndex as (le i)) = (maxHeap (change as (ri i) i) (ri i) (n))
	--- left i é maior que i  e right i 
	|(validIndex (le i) n) && (validIndex (ri i) n) && (getElementByIndex as (le i)) > (getElementByIndex as i) && (getElementByIndex as (le i)) >= (getElementByIndex as (ri i)) = maxHeap (change as (le i) i) (le i) n 
	| (validIndex (le i) n) && (getElementByIndex as (le i)) > (getElementByIndex as i) = (maxHeap (change as (le i) i) (le i) n)
	| otherwise = as 

lacoHeap :: [Int] -> Int -> Int -> [Int]
lacoHeap as i n
	| i >= 1 = lacoHeap (maxHeap as i n) (i-1) (n)  
	| otherwise = as	
	
buildMaxHeap :: [Int] -> Int -> [Int]
buildMaxHeap as n 
	| True = lacoHeap as (div n 2) n

lacoHeapsort :: [Int] -> Int -> Int-> [Int]
lacoHeapsort as i m
	| i >= 2 = lacoHeapsort (maxHeap (change as 1 i) (1) (m-1)) (i-1) (m-1)
	| otherwise = as

heapsort :: [Int] -> [Int]
heapsort as
	| True = lacoHeapsort (buildMaxHeap as ((tamanho as)-1)) ((tamanho as)-1) ((tamanho as)-1)

	
	
	














