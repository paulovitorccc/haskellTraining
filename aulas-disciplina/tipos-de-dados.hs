-- --Escreva a declaracao para o tipo Triple, contendo tres elementos, todos de tipos diferentes.
-- --Escreva funcoes tripleFst, tripleSnd, tripleThr para extrair respectivamente o primeiro, segundo e terceiro
-- -- elementos de uma triple.
-- data Triple a b c = Nada deriving (Eq,Show)

-- tripleFst = undefined
-- tripleSnd = undefined
-- tripleThr = undefined
data Triple a b c = Triple a b c deriving (Eq,Show)
tripleFst (Triple a b c) = a
tripleSnd (Triple a b c) = b
tripleThr (Triple a b c) = c

--------------------------------------------------------------------------
-- --Escreva um tipo Quadruple que contem 4 elementos: dois de um mesmo tipo e outros dois de outro tipo
-- --Escreva as funcoes frstTwo e secondTwo que retornam os dois primeiros e os dois ultimos, respectivamente
-- data Quadruple a b = Vazio

-- firstTwo = undefined
-- secondTwo = undefined

data Quadruple a b = Quadruple a a b b deriving (Eq,Show)

firstTwo (Quadruple a b c d) = (a,b)
secondTwo (Quadruple a b c d) = (c,d)

--------------------------------------------------------------------------
-- --Escreva um tipo de dados que pode conter um, dois, tres ou quatro elementos, dependendo do construtor
-- --Implemente funções tuple1 até tuple4 que que retornam Just <valor> ou Nothing se o valor nao existe
-- data Tuple a b c d = NVazio

-- tuple1 = undefined 
-- tuple2 = undefined 
-- tuple3 = undefined 
-- tuple4 = undefined 

data Tuple a b c d = Tuple4 a b c d | Tuple3 a b c | Tuple2 a b | Tuple1 a deriving (Eq,Show)

tuple1 (Tuple4 a b c d) = Just a 
tuple1 (Tuple3 a b c) = Just a 
tuple1 (Tuple2 a b) = Just a 
tuple1 (Tuple1 a) = Just a 

tuple2 (Tuple4 a b c d) = Just b 
tuple2 (Tuple3 a b c) = Just b 
tuple2 (Tuple2 a b) = Just b 
tuple2 _ = Nothing

tuple3 (Tuple4 a b c d) = Just c 
tuple3 (Tuple3 a b c) = Just c 
tuple3 _ = Nothing 

tuple4 (Tuple4 a b c d) = Just d 
tuple4 _ = Nothing 

--------------------------------------------------------------------------
-- data List a = Nil | Cons a (List a) deriving (Eq,Show)

-- listLength Nil = 0
-- listLength (Cons x xs) = 1 + listLength xs

-- listHead Nil = error "Empty list"
-- listHead (Cons x xs) = x

-- listTail Nil = error "Empty list"
-- listTail (Cons x xs) = xs

-- listFoldr f v Nil = v
-- listFoldr f v (Cons x xs) = f x (listFoldr f v xs)


-- listFoldl f v Nil = v
-- listFoldl f v (Cons x xs) = listFoldl f (f v x) xs 

--------------------------------------------------------------------------
-- --Escreva as funcoes sobre a estrutura de dados binary tree
-- data BinaryTree a = NIL | Node a (BinaryTree a) (BinaryTree a)
--  deriving (Eq,Show)

data BinaryTree a = NIL | Node a (BinaryTree a) (BinaryTree a) deriving (Eq,Show,Ord)

-- sizeBST NIL = 0
-- sizeBST (Node a left right) = 1 + sizeBST left + sizeBST right

sizeBST NIL = 0
sizeBST (Node a left right) = 1 + sizeBST left + sizeBST right

-- --verifica se uma BT é uma BST
-- isBST = undefined

nodeEl (NIL) = NIL
nodeEl (Node a _ _) = a

isBST (NIL) = True
isBST (Node a NIL NIL) = True
isBST bt@(Node a left right)
    | (left == NIL || left <= bt) && (right == NIL || right > bt) = isBST left && isBST right
    | otherwise = False

-- --insere uma nova chave na BST retornando a BST modificada
-- insert = undefined

-- insert NIL _ = NIL 
-- insert b (Node a left right) = 

-- --retorna o Node da BST contendo o dado procurado ou entao NIL
-- search = undefined

-- --retorna o elmento maximo da BST
-- maximum = undefined

-- --retorna o elemento minimo da BST
-- minimum = undefined

-- --retorna o predecessor de um elemento da BST, caso o elemento esteja na BST
-- predecessor = undefined

-- --retorna o sucessor de um elemento da BST, caso o elemento esteja na BST
-- successor = undefined

-- --remove ume lemento da BST
-- remove = undefined

-- --retorna uma lista com os dados da BST nos diversos tipos de caminhamento
-- preOrder = undefined
-- order = undefined
-- postOrder = undefined