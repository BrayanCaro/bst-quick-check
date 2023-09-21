module BST.ModelBased.CorrectSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import BST.Data
import BST.Arbitrary ()
import BST.Operations
import qualified Data.List as L

spec :: Spec
spec = do
  it "prop_NilModel" $ property prop_NilModel
  it "prop_InsertModel" $ property prop_InsertModel
  it "prop_DeleteModel" $ property prop_DeleteModel
  it "prop_UnionModel" $ property prop_UnionModel
  it "prop_FindModel" $ property prop_FindModel

prop_NilModel :: Property
prop_NilModel = toList (nil :: Tree) === [ ]

prop_InsertModel :: Key -> Val -> Tree -> Property
prop_InsertModel k v t =
  toList (insert k v t) === L.insert (k , v ) (deleteKey k $ toList t)

prop_DeleteModel :: Key -> Tree -> Property
prop_DeleteModel k t =
  toList (delete k t) === deleteKey k (toList t)

prop_UnionModel :: Tree -> Tree -> Property
prop_UnionModel t t' =
  -- TODO revisar `on`
  --toList (union t t') === L.sort (L.unionBy ((==) `on` fst) (toList t) (toList t'))
  toList (union t t') === L.sort (L.unionBy (\(a, _) (b, _) -> a == b  ) (toList t) (toList t'))

prop_FindModel :: Key -> Tree -> Property
prop_FindModel k t = find k t === L.lookup k (toList t)

deleteKey k = filter ((/= k) . fst)
