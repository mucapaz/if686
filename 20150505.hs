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

