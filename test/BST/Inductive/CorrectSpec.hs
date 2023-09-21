module BST.Inductive.CorrectSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import BST.Data
import BST.Arbitrary ()
import BST.Operations

spec :: Spec
spec = do
  it "prop_UnionNil1" $ property prop_UnionNil1
  it "prop_UnionInsert" $ property prop_UnionInsert
  it "prop_InsertComplete" $ property prop_InsertComplete
  it "prop_InsertCompleteForDelete" $ property prop_InsertCompleteForDelete
  it "prop_InsertCompleteForUnion" $ property prop_InsertCompleteForUnion

prop_UnionNil1 :: Tree -> Property
prop_UnionNil1 t = union nil t === t

prop_UnionInsert :: Tree -> Tree -> (Key, Val) -> Bool
prop_UnionInsert t t' (k, v) =
  union (insert k v t) t' ~=~ insert k v (union t t')

-- Las 2 propiedades previas son inductivas
-- ¿Pero es cierto que cualquier árbol puede ser construido de inserciones?

insertions :: Tree -> [(Key, Val)]
insertions Leaf = [ ]
insertions (Branch l k v r ) = (k , v ) : insertions l ++ insertions r

-- Y aunque verificamos que podemos obtener el árbol en terminos del insert
-- No es suficiente, por lo que probamos delete, union en las siguientes propiedades
prop_InsertComplete :: Tree -> Property
prop_InsertComplete t = t === foldl (flip $ uncurry insert) nil (insertions t)

prop_InsertCompleteForDelete :: Key -> Tree -> Property
prop_InsertCompleteForDelete k t = prop_InsertComplete (delete k t)

prop_InsertCompleteForUnion :: Tree -> Tree -> Property
prop_InsertCompleteForUnion t t' = prop_InsertComplete (union t t')
