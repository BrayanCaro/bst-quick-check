module BST.ModelBased.BadSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import BST.Data
import BST.Arbitrary ()
import BST.Operations
import qualified Data.List as L

spec :: Spec
spec = do
  it "prop_UnionNil1" $ property prop_InsertModel

prop_InsertModel :: Key -> Val -> Tree -> Property
prop_InsertModel k v t = toList (insert k v t) === L.insert (k, v) (toList t)
