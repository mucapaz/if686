-- Aqui começa o trabalho 04

--q1

--q2

ge :: String -> Char -> Int -> String
ge [] b c =  (show c) ++ [b] 
ge a b c 
	| head a == b = ge(tail a) (b) (c+1) 
	| head a /= b = (show c) ++ [b] ++ ge (a) (head a) (0)
	
nl :: String -> String
nl a = ge (a) (head a) (0)

ls :: Int -> String
ls 0 = "1"
ls a = nl (ls (a-1)) 

lookAndSay :: Enum j => j -> String 
lookAndSay a = ls (fromEnum(a))

--q3




