module Examples.ListSpec (spec) where

import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "Ejemplos con QuickCheck para las listas" $ do
    it "Verifica la reversa de una lista" $ do
      property $ prop_RevRev

    it "Verifica la concatenación reversa de una lista" $ do
      property $ prop_RevApp

    it "verifica la cabeza de una lista con función lambda" $
      property $ \x xs -> head (x:xs) == (x :: Int)

prop_RevRev :: [Int] -> Bool
prop_RevRev xs = reverse (reverse xs) == xs

prop_RevApp :: [Int] -> [Int] -> Bool
prop_RevApp xs ys = reverse (xs ++ ys) == reverse ys ++ reverse xs
