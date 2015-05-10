-- Trabalho 11

import Data.Char

type Cpf = String
type Nome = String
type Pessoa = (Cpf,Nome)
type Hash = [[Pessoa]]

base :: Hash
base = [[("000.000.000-00","Samuel"),("000.000.000-55","Rene")],[("000.000.000-01","Rena")],[],[],[],[],[],[],[],[]]

evaluate :: Cpf -> Int
evaluate as
	| as == [] = 0
	| isDigit (head as) = mod ( digitToInt (head as) + (evaluate (tail as)) ) (10)
	| otherwise = mod (evaluate (tail as)) (10) 

getByCpf :: [Pessoa] -> Cpf -> Maybe Pessoa
getByCpf [] c = Nothing
getByCpf ((x,y):z) c
	| x == c = Just (x,y)
	| otherwise = getByCpf z c

getByEvaluate :: Hash -> Cpf -> Int -> Maybe Pessoa
getByEvaluate h c 0 = getByCpf (head h) (c)  
getByEvaluate h c i = getByEvaluate (tail h) (c) (i-1)

get :: Hash -> Cpf -> Maybe Pessoa
get h c = getByEvaluate (h) (c) (evaluate c)


putByCpf :: [Pessoa] -> Pessoa -> [Pessoa]
putByCpf [] p = [p]
putByCpf as p = [head as] ++ putByCpf (tail as) p

putByEvaluate :: Hash -> Pessoa -> Int -> Hash
putByEvaluate h p 0 = [(putByCpf (head h) p)] ++ (tail h) 
putByEvaluate h p i = [head h] ++ (putByEvaluate (tail h) p (i-1)) 

put :: Hash -> Pessoa -> Hash
put h (a,b) = putByEvaluate (h) (a,b) (evaluate a)

removeByCpf :: [Pessoa] -> Cpf -> Maybe [Pessoa]
removeByCpf [] c = Nothing
removeByCpf ((x,y):z) c
	| x == c = Just z
	| otherwise = ( (>>=)(removeByCpf z c) (\k -> Just (k ++  [(x,y)] ) ))      --  [(x,y)] ++ removeByCpf z c
	
raux :: (Maybe [Pessoa]) -> (Maybe Hash)
raux (Nothing) = Nothing
raux (Just x) = Just [x]

removeByEvaluate :: Hash -> Cpf -> Int -> Maybe Hash
removeByEvaluate h c 0 = (>>=)  (raux (removeByCpf (head h) c))  ( \x ->  Just (x ++ (tail h) ))
removeByEvaluate h c i = (>>=) (removeByEvaluate (tail h) c (i-1)) (\x -> Just ([head h] ++ x) )

remove :: Hash -> Cpf -> Maybe Hash
remove h c = removeByEvaluate h c (evaluate c) 

hasKeyByCpf :: [Pessoa] -> Cpf -> Bool
hasKeyByCpf [] c = False
hasKeyByCpf ((x,y):z) c
	| x == c = True
	| otherwise = hasKeyByCpf z c

hasKeyByEvaluate :: Hash -> Cpf -> Int -> Bool
hasKeyByEvaluate h c 0 = hasKeyByCpf (head h) (c)
hasKeyByEvaluate h c i = hasKeyByEvaluate (tail h) (c) (i-1)

hasKey :: Hash -> Cpf -> Bool
hasKey h c = hasKeyByEvaluate h c (evaluate c) 


--Exercicios IO
import System.IO

shorten:: String -> String
shorten str = "youtube.com/" ++ remove "https://www.youtube.com/watch?v=" str 

remove :: String -> String -> String
remove [] sb = sb
remove (a:sa) (b:sb) = remove sa sb



loop :: Handle -> IO ()
loop h=
	do{
		t <- hIsEOF h;
		if t			
		then
			return ();
		else do{
			url <- hGetLine h;
			putStrLn (shorten url);
			loop h;
		}

	}

main:: IO ()
main = do{
	handle <- openFile "in" ReadMode;
	loop handle;
}

-- Exercicios em sala 

type Queue = [Int]
type Stack = [Int]

newtype State t = State {runState :: (Queue, Stack) -> (t, (Queue , Stack))} 


instance Monad State where
	return t = State $ \(st,qu) -> (t,(st,qu))
	(>>=) (State st) (f) = State $ \(s,q) -> let
					(x,(newS,newQ)) = st (s,q)
					(State newState) = f x 
					in newState (newS,newQ)

pop :: State Int
pop = State $ (\(a:as,bs) -> (a, (as,bs)))

push :: Int -> State ()
push x = State $ (\(as,bs) -> ((), (as++[x],bs))) 

tStack :: State ()
tStack = do{
	push 10;
	push 20;
	pop;
	push 30;
	}

tStack2 :: State Int
tStack2 = do{
	push 10;
	push 20;
	insert 1;
	insert 2;
	insert 3;
	remove;
	pop;

	}	

main = putStrLn (show (runState tStack ([],[])))
main2 = putStrLn (show (runState tStack2 ([],[])))


insert :: Int -> State ()
insert x = State $ (\(as,bs) -> ((), (as,x:bs)) ) 

remove :: State Int
remove = State $ (\(as,x:bs) -> (x,(as,bs)))


-- exercicio 2 do trabalho 11

pro :: String -> Maybe String
pro [] = Nothing
pro (a:[])
	| (a < 'a' || a > 'z') && a /= ' ' = Nothing 
	| otherwise = Just [a]
pro (a:as)
	| (a < 'a' || a > 'z') && a /= ' ' = Nothing 
	| otherwise = (>>=) (pro as) (\x -> Just ([a] ++ x))

preAux :: String -> String-> [Maybe String]
preAux [] ar = [Just ar]
preAux (a:as) ar
		| a == ' ' = [Just ar] ++ preAux as "" 
		| otherwise = preAux as (ar++[a]) 
		

pre :: Maybe String -> [Maybe String]
pre Nothing = [Nothing]
pre (Just x) = preAux x "" 
	
imprime :: [Maybe String] -> IO()
imprime (Nothing:as) = putStr "\n"
imprime [] = putStr ""
imprime ((Just x):as)= do {
			putStrLn x;
			imprime as;
		}
	
resolve ::String -> IO()
resolve as = do {
			imprime ( pre (pro as));
		}

f :: IO ()
f = do{
	line <- getLine;
	resolve(line);	
	f;
}
