{-# LANGUAGE ScopedTypeVariables #-}

module BST.Postconditions.PostSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import BST.Data
import BST.Operations
import Control.Applicative ((<|>))

spec :: Spec
spec = do
  it "InsertPost" $ do
    property $ prop_InsertPost
  it "InsertPostSameKey" $ do
    property $ prop_InsertPostSameKey
  it "UnionPost" $ do
    property $ prop_UnionPost
  it "FindPostAbsent" $ do
    property $ prop_FindPostAbsent
  it "FindPostPresent" $ do
    property $ prop_FindPostPresent
  it "InsertDeleteComplete" $ do
    property $ prop_InsertDeleteComplete

instance (Ord k, Arbitrary k, Arbitrary v) => Arbitrary (BST k v) where
  arbitrary = do
    items <- arbitrary
    return $ foldr (uncurry insert) Leaf (items :: [(k,v)])
  shrink = filter valid . genericShrink

prop_InsertPost :: Key -> Val -> Tree -> Key -> Property
prop_InsertPost k v t k'
  = find k' (insert k v t) === if k == k' then Just v else find k' t

prop_InsertPostSameKey :: Key -> Val -> Tree -> Property
prop_InsertPostSameKey k v t = prop_InsertPost k v t k

prop_UnionPost :: Tree -> Tree -> Key -> Property
prop_UnionPost t t' k = find k (union t t') === (find k t <|> find k t')

prop_FindPostPresent :: Key -> Val -> Tree -> Property
prop_FindPostPresent k v t = find k (insert k v t) === Just v

prop_FindPostAbsent :: Key -> Tree -> Property
prop_FindPostAbsent k t = find k (delete k t) === Nothing

{-
  De las dos propiedades definidas previamente,
  vale la pena agruparlas en una.
  Ya que la forma anterior nos obliga a definir las propiedades una
  por una, y caso por caso de forma exhaustiva, pero puede que no cumplamos
  todos los casos para cualquier estructura.

  Nota: Aunque para el tipo de dato Tree se
  pueden considerar solo esos dos casos y por eso son tan similares las dos propiedades
  definidas previamente.
-}
prop_InsertDeleteComplete :: Key -> Tree -> Property
prop_InsertDeleteComplete k t = case find k t of
    Nothing -> t === delete k t
    Just v -> t === insert k v t
