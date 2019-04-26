{-
Questão 1 
A função de Fibonacci, implementada em haskell, comumente possui duas recursões. 
Implemente esta função de uma maneira mais eficiente, contendo apenas uma recursão. 
fib x = undefined
-}

fibAux 0 list = last list
fibAux x list = fibAux (x-1) (list++[(f1+f2)])
    where f1 = last list
          f2 = last (init list) 

fib 0 = 0 
fib 1 = 1
fib x = fibAux (x-1) [0,1]

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

a) search - que retorna True se o elemento x está na bst e False 
caso contrário.
b) leaves - retorna uma lista com todas as folhas da bst
c) mirror - retorna a bst com seus filhos trocados/espelhada
d) profundidade - retorna a profundidade de um elemento x, se não 
fazer parte da lista retorna -1.
e) mapTree - aplica uma função f a cada elemento da bst.
-}