{-
Questão 1 
A função de Fibonacci, implementada em haskell, comumente possui duas recursões. 
Implemente esta função de uma maneira mais eficiente, contendo apenas uma recursão. 
fib x = undefined
-}

{-
Questão 2
Implemente uma função que recebe como parâmetro uma lista e um numero k, e 
retorna uma lista com todos os elementos que ocorrem pelo menos k vezes na lista.
Ex: repeteMin [1,2,3,4,4,5,6,7,5] 2 -> [4,5]
repeteMin list k = undefined
-}

{-
Questão 3
Implemente as seguintes funções operando com uma bst:
Construtor da bst: 
data BinaryTree a = NIL | Node a (BinaryTree a) (BinaryTree a) deriving (Eq,Show,Ord)

a) search 
b) 
c) mirror
d) profundidade
e) mapTree
-}