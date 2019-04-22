data BinaryTree a = NIL | Node a (BinaryTree a) (BinaryTree a) deriving (Eq,Show,Ord)

-- sizeBST NIL = 0
-- sizeBST (Node a left right) = 1 + sizeBST left + sizeBST right

sizeBST NIL = 0
sizeBST (Node a left right) = 1 + sizeBST left + sizeBST right

-- --verifica se uma BT é uma BST
-- isBST = undefined

isBST (NIL) = True
isBST (Node a NIL NIL) = True
isBST bt@(Node a left right)
    | (left == NIL || left < bt) && (right == NIL || right > bt) = isBST left && isBST right
    | otherwise = False

-- --insere uma nova chave na BST retornando a BST modificada
-- insert = undefined
insert x NIL = Node x NIL NIL
insert x bt@(Node a left right)
    | x < a = Node a (insert x left) right
    | x > a = Node a left (insert x right)
    | otherwise = Node x left right

-- --retorna o Node da BST contendo o dado procurado ou entao NIL
-- search = undefined
search _ (NIL) = NIL
search x bst@(Node a left right)
    | x < a = search x left
    | x > a = search x right
    | x == a = bst

-- --retorna o elmento maximo da BST
-- maximum = undefined
-- maximumAux x (NIL) = x 
-- maximumAux x (Node a left NIL) = maximumAux a right

-- maximum' (NIL) = NIL
-- maximum' bst@(Node a _ NIL) = a
-- maximum' bst@(Node a left right) = maximum' right

-- --retorna o elemento minimo da BST
-- minimum = undefined

-- --retorna o predecessor de um elemento da BST, caso o elemento esteja na BST
-- predecessor = undefined

-- --retorna o sucessor de um elemento da BST, caso o elemento esteja na BST
-- successor = undefined

-- --remove ume lemento da BST
-- remove = undefined

-- --retorna uma lista com os dados da BST nos diversos tipos de caminhamento
-- order = undefined
-- preOrder = undefined
-- postOrder = undefined
order (NIL) = print ""
order (Node a left right) = order left ++ print a ++ order right

preorder (NIL) = []
preorder (Node a left right) = [a] ++ order left ++ order right

postorder (NIL) = []
postorder (Node a left right) = order left ++ order right ++ [a]