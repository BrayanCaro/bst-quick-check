{-# LANGUAGE ScopedTypeVariables #-}

module BST.Arbitrary where

import BST.Data
import Test.QuickCheck
import BST.Operations

instance (Ord k, Arbitrary k, Arbitrary v) => Arbitrary (BST k v) where
  arbitrary = do
    items <- arbitrary
    return $ foldr (uncurry insert) Leaf (items :: [(k,v)])
  shrink = filter valid . genericShrink
