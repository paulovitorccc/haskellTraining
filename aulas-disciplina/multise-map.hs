module MultisetMap (insert,
                    remove,
                    search,
                    union,
                    intersection,
                    minus,
                    inclusion,
                    MultisetMap.sum,
                    size)
 where

{- 
 - Um multi-conjunto (ou bag) é uma estrutura que representa uma coleção de objetos que 
 - permite duplicadas. Entretanto, as duplicatas são armazenadas como a quantidade de 
 - ocorréncias do mesmo elemento no multi-conjunto. Exemplo, a coleção {a,b,c,c,c,b} poderia 
 - ser representada como sendo {(a,1), (b,2), (c,3)}. A ideia de multi-conjunto pode ser 
 - implementada de diversas formas. Uma delas é usando a implementacao de Data.Map, onde 
 - cada elemento da lista consiste do dado em si mapeado para sua quantidade. 
 - Eh recomendavel que voce consulte a documentacao de Data.Map
 -}
import qualified Data.Map as Map
import qualified Data.List as List

{-
 - Insere um elemento na estrutura. Caso o elemento ja existe, sua quantidade na estrutura sera incrementada.
 -}
insert elem bag
    | bag == Map.empty = Map.fromList [(elem,1)]
    | isElement = Map.adjust (1 +) elem bag
    | otherwise = Map.insert elem 1 bag
    where isElement = List.elem elem (Map.keys bag) 

{-
- Remove um elemento da estrutura, levando em consideracao a manipulacao de sua quantidade na estrutura. 
- Caso a quantidade atinja 0 (ou menos), o elemento deve realmente ser removido da estrutura
-}
remove elem bag
    | elemWithKey == Map.empty = bag
    | otherwise = if (elemWithKey Map.! elem == 1) 
        then Map.delete elem bag 
        else Map.adjust (-1 +) elem bag   
    where elemWithKey = Map.filterWithKey (\k _ -> k == elem) bag

{-
 - Busca um elemento na estrutura retornando sua quantidade. Caso o elemento nao exista, retorna 0 como a quantidade.
-}
search elem bag
    | elemWithKey == Map.empty = 0
    | otherwise = elemWithKey Map.! elem  
    where elemWithKey = Map.filterWithKey (\k _ -> k == elem) bag


{-
 - Faz a uniao deste Bag com otherBag. A uniao consiste em ter os elementos dos dois Bags com suas maiores quantidades.
 - Por exemplo, A = {(a,1),(c,3)}, B = {(b,2),(c,1)}. A.union(B) deixa A = {(a,1),(c,3),(b,2)}
-}
fUnion _ l r = (max l r)
union bag1 bag2 = Map.unionWithKey fUnion bag1 bag2

{-
 - Faz a intersecao deste Bag com otherBag. A intersecao consiste em ter os elementos que estao em ambos os bags com suas 
 - menores quantidades. Por exemplo, Seja A = {(a,3),(b,1)} e B = {(a,1)}. Assim, A.intersection(B) deixa A = {(a,1)}
 - Caso senhum elemento de A esteja contido em B ent�o a intersecao deixa A vazio.
-}
fIntersection _ l r = (min l r)
intersection bag1 bag2 = Map.intersectionWithKey fIntersection bag1 bag2

{-
 - Faz a diferenca deste Bag com otherBag. A diferenca A \ B entre bags eh definida como segue:
   - contem os elementos de A que nao estao em B
   - contem os elementos x de A que estao em B mas com sua quantidade subtraida (qtde em A - qtde em B). 
     Caso essa quantidade seja negativa o elemento deve serremovido do Bag. 
     Por exemplo, seja A = {(a,3),(b,1)} e B = {(b,2),(a,1)}. Assim, A.minus(B) deixa A = {(a,2)}.
-}
fInterMinus _ l r = l-r
minus bag1 bag2 = Map.filter (> 0) bagMinus
    where bagMinus = union (bag1 Map.\\ bag2) (Map.intersectionWithKey fInterMinus bag1 bag2) 

{-
 - Testa se este Bag esta incluso em otherBag. Para todo elemento deste bag, sua quantidade
 - deve ser menor or igual a sua quantidade em otherBag.
-}
fInterInc k l r = r-l
inclusion bag1 bag2
    | bSub == Map.empty = if (filterInter == Map.empty) then True else False
    | otherwise = False 
    where bSub = bag1 Map.\\ bag2
          bInterInc = Map.intersectionWithKey fInterInc bag1 bag2
          filterInter = Map.filter (< 0) bInterInc

{-
 - Realiza a soma deste Bag com otherBag. A soma de dois bags contem os elementos dos dois bags com suas quantidades somadas. 
-}
fSum k l r = l + r
sum bag1 bag2 = Map.unionWithKey fSum bag1 bag2

{-
 - Retorna a quantidade total de elementos no Bag
-}
size bag = Map.size bag