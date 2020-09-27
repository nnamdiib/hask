-- Author: nnamdiib
-- Implemented as specified in "Left-leaning Red-black Trees" (Sedgewick), https://www.cs.princeton.edu/~rs/talks/LLRB/LLRB.pdf

module Tree.RedBlack(
    singletonTree,
    treeInsert,
    treeElem
) where

data Colour = Red | Black deriving (Show, Read, Eq)

data RedBlackTree a = EmptyTree | Node a Colour (RedBlackTree a) (RedBlackTree a) deriving (Show, Read)

singletonTree :: a -> RedBlackTree a
singletonTree x = Node x Red EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> RedBlackTree a -> RedBlackTree a
treeInsert val EmptyTree = singletonTree val
treeInsert val (Node x colour left right)
  | val < x = postInsertionCheck $ Node x colour (treeInsert val left) right
  | val == x = Node val colour left right
  | val > x = postInsertionCheck $ Node x colour left (treeInsert val right)

treeElem :: (Ord a) => a -> RedBlackTree a -> Bool
treeElem _ EmptyTree = False
treeElem val (Node x _ left right)
    | val == x = True
    | val < x = treeElem val left
    | val > x = treeElem val right

postInsertionCheck :: RedBlackTree a -> RedBlackTree a
postInsertionCheck = colourFlip . rightRotate . leftRotate

rightRotate :: RedBlackTree a -> RedBlackTree a
rightRotate node@(Node _ _ EmptyTree _) = node
rightRotate node@(Node val colour left@(Node leftVal leftColour leftLeft leftRight) right)
    | (nodeIsRed left) && (nodeIsRed leftLeft) = Node leftVal colour leftLeft (Node val leftColour leftRight right)
    | otherwise = node

leftRotate :: RedBlackTree a -> RedBlackTree a
leftRotate node@(Node _ _ _ EmptyTree) = node
leftRotate node@(Node val colour left right@(Node rightVal rightColour rightLeft rightRight))
    | (nodeIsRed right) && (not . nodeIsRed $ left) = Node rightVal colour (Node val rightColour left rightLeft) rightRight
    | otherwise = node

colourFlip :: RedBlackTree a -> RedBlackTree a
colourFlip node@(Node val _ left right)
    | (nodeIsRed left) && (nodeIsRed right) = Node val Red (flipNodeColour left) (flipNodeColour right)
    | otherwise = node

flipNodeColour :: RedBlackTree a -> RedBlackTree a
flipNodeColour (Node val Black left right) = Node val Red left right
flipNodeColour (Node val Red left right) = Node val Black left right

nodeIsRed :: RedBlackTree a -> Bool
nodeIsRed EmptyTree = False
nodeIsRed (Node _ colour _ _) = colour == Red
