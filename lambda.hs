-- pow x y = undefined
pow = \x y -> product (replicate y x)

-- fatorial x = undefined
fatorial = \x -> product [1..x]

-- isPrime x = undefined
primeAux = \a b -> if(b == 1) 
    then True 
    else if((mod a b) == 0) 
        then False 
        else primeAux a (b-1)
isPrime = \a -> if (a == 1) then True else primeAux a (a-1)

-- fib x = undefined
fib = \x -> if (x==0) then 0 
    else if (x==1) then 1 
        else fib(x-1) + fib (x-2)

-- mdc x y = undefined
mdcAux = \x y -> if (y==0) then x else mdcAux y (mod x y)
mdc = \x y -> if(x >= y) then mdcAux x y else mdcAux y x

-- mmc x y = undefined
-- mmc = \x y -> (x*y) / (mdc x y)

-- coprimo x y = undefined
-- goldbach x = undefined

-- meuLast xs = undefined
meuLast = \x -> if (length x == 0) then error "Lista vazia!" else last x

-- penultimo xs = undefined
penultimo = \xs -> if (length xs == 0) then error "Lista sem penultimo"
    else if (length xs == 1) then error "Lista sem penultimo"
        else last (init xs)

-- elementAt i xs = undefined
elementAt = \i xs -> xs !! (i-1)

-- meuLength xs = undefined
meuLength = \xs -> if (length xs == 0) then 0 else 1 + meuLength (tail xs) 

-- meuReverso xs = undefined
meuReverso = \xs -> if (length xs == 0) 
    then [] 
    else last xs : meuReverso (init xs)

-- isPalindrome xs = undefined
isPalindromeAux = \xs ys -> 
	if (null xs && null ys) then True
	else if (null xs || null ys) then False
	else if (head xs /= head ys) then False 
	else isPalindromeAux (tail xs) (tail ys)
isPalindrome = \xs -> isPalindromeAux xs (meuReverso xs)

-- compress xs = undefined
-- compact xs = undefined
-- encode xs = undefined
-- split xs i = undefined
-- slice xs imin imax = undefined
-- insertAt el pos xs = undefined
-- sort xs = undefined
-- mySum xs = undefined
-- maxList xs = undefined
-- buildPalindrome xs = undefined
-- mean xs = undefined
-- myAppend xs ys = undefined