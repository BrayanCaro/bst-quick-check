{-# LANGUAGE ScopedTypeVariables #-}

module BST.Metamorphic.BadSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import BST.Data
import BST.Arbitrary
import BST.Operations

spec :: Spec
spec = do
  it "prop_InsertInsert" $ do
    property $ prop_InsertInsert

-- La siguiente propiedad falla, pues no hemos definido
-- relaciones de equivalencia
prop_InsertInsert :: (Key, Val) -> (Key, Val) -> Tree -> Property
prop_InsertInsert (k , v ) (k', v') t =
  insert k v (insert k' v' t) === insert k' v' (insert k v t)

