vendas :: Int -> Int 
vendas n = 1

func :: Int -> Int -> Int
func s n 
		| (n == 0) && (vendas(n) == s) = 1
		| (n == 0) && (vendas(n) /= s) = 0
		| vendas(n) == s = (func (s) (n-1)) + 1
		| vendas(n) /= s = (func (s) (n-1))
