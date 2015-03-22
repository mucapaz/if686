	

    double :: [Int] -> [Int]
    double a
                    | a == [] = []
                    | otherwise = [head a + head a] ++ double(tail a)
                   
                   
                   
    member :: [Int] -> Int -> Bool
    member a k
                    | a == [] = False
                    | (head a == k) = True
                    | (head a /= k) = member (tail a) k
                   
    digits :: String -> String
    digits s
                    | s == [] = []
                    | (head s >= '0') && (head s <= '9') = [head s] ++ digits(tail s)
                    | otherwise = digits(tail s)
                   
    sumPairs :: [Int] -> [Int] -> [Int]
    sumPairs s v
                    | (s == []) = v
                    | (v == []) = s
                    | otherwise = (head s + head v):(sumPairs (tail s) (tail v))
     
    menor :: [Int] -> Int -> [Int]
    menor l x
             | l == [] = []
             | head l < x = [head l] ++ menor (tail l) (x)
             | otherwise = menor(tail l) (x)
             
    maior :: [Int] -> Int -> [Int] 
    maior l x
             | l == [] = []
             | head l >= x = [head l] ++ maior (tail l) (x)
             | otherwise = maior(tail l) (x)       
                   
    qsort :: [Int] -> [Int]
    qsort l
                    | l == [] = []
                    | otherwise = qsort(menor(tail l)(head l)) ++ [head l] ++ qsort(maior(tail l)(head l))

