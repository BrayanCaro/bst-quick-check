module BST.Metamorphic.CorrectSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import BST.Data
import BST.Arbitrary ()
import BST.Operations

spec :: Spec
spec = do
  it "prop_InsertInsertSameKey" $ property prop_InsertInsertSameKey
  it "prop_InsertDelete" $ property prop_InsertDelete
  it "prop_InsertUnion" $ property prop_InsertUnion

-- Con equivalencia

prop_InsertInsertSameKey :: (Key, Val) -> (Key, Val) -> Tree -> Bool
prop_InsertInsertSameKey (k , v) (k', v') t =
    insert k v (insert k' v' t) ~=~
        if k == k'
           then insert k v t
           else insert k' v' (insert k v t)

prop_InsertDelete :: (Key, Val) -> Key -> Tree -> Bool
prop_InsertDelete (k, v) k' t =
      insert k v (delete k' t) ~=~
        if k == k'
           then insert k v t
           else delete k' (insert k v t)

prop_InsertUnion :: (Key, Val) -> Tree -> Tree -> Bool
prop_InsertUnion (k, v) t t' =
  insert k v (union t t') ~=~ union (insert k v t) t'
