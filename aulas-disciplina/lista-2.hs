-- Lista 1

-- Questão 1
-- - Dada uma tupla, divide o primeiro pelo segundo usando pattern matching.
-- - Ela deve ser indefinida quando o denominador for zero.
-- divTuple (x, y) = undefined
divTuple (_, 0) = undefined
divTuple (x, y) = x/y

-- Questão 2
--  - Calcula o somatorio entre dois numeros a e b (a < b). Procure usar alguma funcao pronta sobre listas. 
--  - Ex: somatorio 0 1 = 1
--  -     somatorio 1 3 = 6
-- somatorio a b = undefined 
somatorio a b = sum [a..b]

-- Questão 3
--  - Calcula o somatorio (recursivo) entre dois numeros a e b (a < b).
--  - Ex: somatorio 0 1 = 1
--  -     somatorio 1 3 = 6
-- somatorioRec a b = undefined
somatorioRec a b 
    | a == b = a
    | otherwise = a + somatorioRec (a+1) b

-- Questão 4
-- - Defina a funcao que eleva um membro ao quadrado
-- square x = undefined 
square x = x*x

-- Questão 5
-- - Soma os quadrados de dois numeros.
-- sumSquares x y = undefined
sumSquares x y = square x + square y

-- Questão 6
-- - Defina uma funcao de alta ordem que aceita uma função (Int -> Int) e aplica a funcao a dois numeros
-- higherOrderSum f a b = undefined
higherOrderSum f a b = (f a) + (f b)

-- Questão 7
-- - Defina a soma dos auqdrados em termos de higherOrderSum
-- hoSumSquares = undefined
hoSumSquares a b = higherOrderSum square a b

-- Questão 8
-- - Implemente a funcao mapFilter que primeiro aplica o map de uma funcao f a uma lista e depois aplica a funcao filter
-- - a lista resultante. Procure usar a composicao de funcoes
-- mapFilter f p xs = undefined
mapFilter f p xs = filter p (map f xs)

---------------------------------------------------------

-- Lista 2

-- Questão 1
-- {-
-- - Encontra o ultimo elemento de uma lista. Caso a lista seja vazia retorne o seguinte comando: error "Lista vazia!" 
-- -}
-- meuLast xs = undefined
meuLast [] = error "Lista vazia!"
meuLast xs = last xs

-- Questão 2
-- {-
-- - Encontra o penultimo elemento de uma lista. Caso a lista seja vazia ou tenha apenas um elemento retorne o seguinte comando: error "Lista sem penultimo" 
-- -}
-- penultimo xs = undefined
penultimo []    = error "Lista sem penultimo"
penultimo (xs:[]) = error "Lista sem penultimo"
penultimo xs    = last (init xs)

-- Questão 3
-- {-
-- - Retorna o k-esimo (k varia de 1 ate N) elemento de uma lista. Ex: elementAt 2 [4,7,1,9] = 7
-- -}
-- elementAt i xs = undefined
elementAt i xs = xs !! (i-1)

-- Questão 4
-- {-
-- - Retorna o tamanho de uma lista. 
-- -}
-- meuLength xs = undefined
meuLength [] = 0
meuLength (x:xs) = 1 + meuLength xs 

-- Questão 5
-- {-
-- - Retorna o inverso de uma lista. 
-- -}
-- meuReverso xs = undefined
meuReverso [] = []
meuReverso xs = last xs : meuReverso (init xs)

-- Questão 6
-- {-
-- - Diz se uma lista é palindrome. 
-- -}
-- isPalindrome xs = undefined
isPalindromeAux [] [] = True
isPalindromeAux [] _ = False
isPalindromeAux _ [] = False
isPalindromeAux (x:xs) (y:ys) = if (x /= y) then False else isPalindromeAux xs ys
isPalindrome xs = isPalindromeAux xs (meuReverso xs)

-- Questão 7
-- {-
-- - Remove os elementos duplicados de uma lista. Ex: compress [2,5,8,2,1,8] = [2,5,8,1]
-- - Voce pode usar a funcao elem de Haskell
-- -}
-- compress xs = undefined
compressAux [] r = r
compressAux (x:xs) r 
    | elem x r = compressAux xs r
    | otherwise = compressAux xs (r++[x])
compress xs = compressAux xs []

-- Questão 8
-- {-
-- - Varre a lista da esquerda para a direita e junta os elementos iguais. Ex: compact [2,5,8,2,1,8] = [2,2,5,8,8,1]
-- - Voce pode usar funcoes sobre listas como : (cons), filter, etc.
-- -}
-- compact xs = undefined
verifyEquals x ys = filter (== x) ys
compactAux [] r = r
compactAux (x:xs) r
    | elem x r = compactAux xs r
    | otherwise = compactAux xs (r ++ (x : (verifyEquals x xs)))  
compact xs = compactAux xs []

-- Questão 9
-- {-
-- - Retorna uma lista de pares com os elementos e suas quantidades. Ex: encode [2,2,2,3,4,2,5,2,4,5] = [(2,5),(3,1),(4,2),(5,2)]
-- - Voce pode usar funcoes sobre listas como : (cons), filter, etc.
-- -}
-- encode xs = undefined
encode xs = compress [(y, meuLength k) | y <- xs, let k = verifyEquals y xs]

{-
- Divide uma lista em duas sublistas onde o ponto de divisao é dado. Ex: split [3,6,1,9,4] 3 = [[3,6,1],[9,4]]
-}
splitInit [] _ = []
splitInit xs 0 = []
splitInit (x:xs) i = x : splitInit xs (i-1)

splitLast [] _ = []
splitLast xs 0 = xs
splitLast (x:xs) i = splitLast xs (i-1)

split xs i = [splitInit xs i, splitLast xs i]

{-
- Extrai um pedaço (slice) de uma lista especificado por um intervalo. 
- Ex: slice [3,6,1,9,4] 2 4 = [6,1,9]
-}
sliceAux [] _ _ _ = []
sliceAux (x:xs) i imin imax
    | i > imax = []
    | i >= imin = x : sliceAux xs (i+1) imin imax
    | otherwise = sliceAux xs (i+1) imin imax

slice xs imin imax = sliceAux xs 1 imin imax 

{-
- Insere um elemento em uma posicao especifica de uma lista. 
- Ex: insertAt 7 4 [3,6,1,9,4] = [3,6,1,7,9,4]
-}
insertAt el _ [] = [el]
insertAt el 1 xs = el : xs
insertAt el pos (x:xs) = x : insertAt el (pos-1) xs

{-
- Ordena uma lista em ordem crescente. Voce deve seguir a ideia do selectionsort onde os elementos 
- menores sao trazidos para o inicio da lista um a um. Esta funcao ja esta implementada.
-}
minList [x] = x
minList (x:xs) = if (x < (minList xs)) then x else minList xs

remove e (x:xs) | e == x = xs
                | otherwise = x:(remove e xs)
sort [] = []
sort xs = x:ys 
    where
        x = minList xs
        ys = sort (remove x xs) 

{-
- Calcula a soma de todos os elementos de uma lista usando foldr.
-}
mySum xs = foldr (+) 0 xs

{-
- Dada a funcao max que retorna o maximo entre dois numeros, escreva uma funcao que usa a função
- foldr e max para retornar o maximo de uma lista se a lista não é vazia.
-}
maxList xs = foldr (\x y -> max x y) 0 xs

{-
- Transforma uma string em uma palindrome acrescentando o reverso da string ao seu final sem usar a funcao reverse. 
- Ex: buildPalindrome [1,2,3] = [1,2,3,3,2,1]. 
-}
buildPalindromeAux [] = []
buildPalindromeAux xs = last xs : buildPalindromeAux (init xs)

buildPalindrome xs = xs ++ buildPalindromeAux xs

{-
- Computa a media dos elementos de uma lista de numeros, sem usar nenhuma funcao pronta de listas.
-}
mean xs = undefined

{-
- Escreva a funcao myAppend que faz o append de uma lista xs com a lista ys, usando a função foldr. 
-}
myAppend xs ys = undefined