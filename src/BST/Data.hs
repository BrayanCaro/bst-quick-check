{-# LANGUAGE DeriveGeneric #-}

module BST.Data (BST(Leaf, Branch)) where

import GHC.Generics

data BST k v =
    Leaf |
    Branch (BST k v) k v (BST k v)
    deriving (Eq, Show, Generic)

