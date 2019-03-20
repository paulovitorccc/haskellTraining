-- Lista 1
-- Questão 1
divTuple (_, 0) = undefined
divTuple (x, y) = x/y

-- Questão 2
somatorio a b = sum [a..b]

-- Questão 3
somatorioRec a b 
    | a == b = a
    | otherwise = a + somatorioRec (a+1) b

-- Questão 4
square x = x*x

-- Questão 5
sumSquares x y = square x + square y

-- Questão 6
higherOrderSum f a b = (f a) + (f b)

-- Questão 7
hoSumSquares a b = higherOrderSum square a b

-- Questão 8
mapFilter f p xs = filter p (map f xs)

---------------------------------------------------------

-- Lista 2
-- Questão 1
meuLast [] = error "Lista vazia!"
meuLast xs = last xs

-- Questão 2
penultimo []    = error "Lista sem penultimo"
penultimo (xs:[]) = error "Lista sem penultimo"
penultimo xs    = last (init xs)

-- Questão 3
elementAt i xs = xs !! (i-1)

-- Questão 4
meuLength [] = 0
meuLength (x:xs) = 1 + meuLength xs 

-- Questão 5
meuReverso [] = []
meuReverso xs = last xs : meuReverso (init xs)

-- Questão 6
isPalindromeAux [] [] = True
isPalindromeAux [] _ = False
isPalindromeAux _ [] = False
isPalindromeAux (x:xs) (y:ys) = if (x /= y) then False else isPalindromeAux xs ys
isPalindrome xs = isPalindromeAux xs (meuReverso xs)

-- Questão 7
compressAux [] r = r
compressAux (x:xs) r 
    | elem x r = compressAux xs r
    | otherwise = compressAux xs (r++[x])
compress xs = compressAux xs []

-- Questão 8
verifyEquals x ys = filter (== x) ys
compactAux [] r = r
compactAux (x:xs) r
    | elem x r = compactAux xs r
    | otherwise = compactAux xs (r ++ (x : (verifyEquals x xs)))  
compact xs = compactAux xs []

-- Questão 9
encode xs = compress [(y, meuLength k) | y <- xs, let k = verifyEquals y xs]

-- Questão 10
split xs i = [take i xs] ++ [drop i xs]

