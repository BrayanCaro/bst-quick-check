{-# LANGUAGE DeriveGeneric #-}

module BST.Data (BST(Leaf, Branch), Tree, Key, Val) where

import GHC.Generics

type Key = Int
type Val = Int
type Tree = BST Key Val

data BST k v =
    Leaf |
    Branch (BST k v) k v (BST k v)
    deriving (Eq, Show, Generic)
