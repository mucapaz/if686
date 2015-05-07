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



