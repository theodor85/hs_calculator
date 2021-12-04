module BinaryTree
    ( 
      BinaryTree
    ) where


data BinaryTree a = Leaf a | Branch a (BinaryTree a) (BinaryTree a) | EmptyNode
  deriving Show
