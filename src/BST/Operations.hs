module BST.Operations (insert
  , delete
  , union
  , nil
  , keys
  , toList
  , valid
  , find
  ) where

import Control.Applicative ((<|>))
import BST.Data

insert :: Ord k => k -> v -> BST k v -> BST k v
insert k v Leaf = Branch Leaf k v Leaf
insert k v (Branch t1 k' v' t2)
  | k < k' = Branch (insert k v t1) k' v' t2
  | k > k' = Branch t1 k' v' (insert k v t2)
  -- Si las llaves son iguales: la última inserción gana
  -- (como dice en las notas, pag. 68)
  | otherwise = Branch t1 k v t2

find :: Ord k => k -> BST k v -> Maybe v
find _ Leaf = Nothing
find k (Branch t1 k' v t2)
  | k > k' = Nothing
  | k < k' = find k t1 <|> find k t2
  | otherwise = Just v

delete :: Ord k => k -> BST k v -> BST k v
delete _ Leaf = Leaf
delete k (Branch t1 k' v t2)
  | k > k' = Branch t1 k' v t2
  | k < k' = Branch (delete k t1) k' v (delete k t2)
  | otherwise = Leaf

union :: (Ord k) => BST k v -> BST k v -> BST k v
union t1 Leaf = t1
union Leaf t2 = t2
union (Branch t1 k1 v1 t1') (Branch t2 k2 v2 t2')
  | k1 > k2 = Branch (Branch t2 k2 v2 t2') k1 v1  Leaf
  | k2 > k1 = Branch (Branch t1 k1 v1 t1') k2 v2  Leaf
  | otherwise =  Branch t1 k1 v1 t1'

nil :: BST k v
nil = Leaf


-- Funciones auxiliares

keys :: BST k v -> [k]
keys Leaf = []
keys (Branch t1 k _ t2) = [k] ++ (keys t1) ++ (keys t2)

toList :: BST k v -> [(k ,v)]
toList Leaf = []
toList (Branch t1 k v t2) = [(k, v)] ++ (toList t1) ++ (toList t2)

valid :: Ord k => BST k v -> Bool
valid Leaf = True
valid (Branch l k _ r ) =
  valid l &&
  valid r &&
  all (<k) (keys l) &&
  all (>k) (keys r)
