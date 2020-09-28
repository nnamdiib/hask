-- Implementation of an unbalanced search tree.
module Tree.BST(
    singletonTree,
    treeInsert,
    treeElem
) where

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert val EmptyTree = singletonTree val
treeInsert val (Node x left right)
  | val < x = Node x (treeInsert val left) right
  | val == x = Node val left right
  | val > x = Node x left (treeInsert val right)

singletonTree :: a -> Tree a
singletonTree x = Node x EmptyTree EmptyTree

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node val left right) 
    | x < val = treeElem x left
    | x == val = True
    | x > val = treeElem x right