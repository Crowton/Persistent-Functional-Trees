{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module BinaryTreeEphemeral where

import DataRecords

import Prelude hiding (sum)


getFunc :: Ord s => EPH_BST s s
getFunc = (Leaf, insert, delete)


contains :: Ord e => e -> Tree e -> Bool
contains _ Leaf = False
contains e Node {elm=elm, children=[left, right]}
  | e == elm = True
  | e < elm = contains e left
  | otherwise = contains e right


sum :: Tree Int -> Int
sum Leaf = 0
sum Node {elm=elm, children=[left, right]} = (sum left) + elm + (sum right)


insert :: Ord e => e -> Tree e -> Tree e
insert e Leaf = Node {elm=e, children=[Leaf, Leaf]}
insert e Node {elm=elm, children=[left, right]}
  | e == elm = Node {elm=elm, children=[left, right]}
  | e < elm = Node {elm=elm, children=[insert e left, right]}
  | otherwise = Node {elm=elm, children=[left, insert e right]}


extractMax :: Ord e => Tree e -> (Tree e, Maybe e)
extractMax Leaf = (Leaf, Nothing)
extractMax Node {elm=elm, children=[left, Leaf]} = (left, Just elm)
extractMax Node {elm=elm, children=[left, right]} =
    let (right', max) = extractMax right in
    (Node {elm=elm, children=[left, right']}, max)

delete :: Ord e => e -> Tree e -> Tree e
delete _ Leaf = Leaf
delete e Node {elm=elm, children=[left, right]}
  | e == elm = let (left', max) = extractMax left in
               case max of
                   Nothing -> right
                   Just elm' -> Node {elm=elm', children=[left', right]}
  | e < elm = Node {elm=elm, children=[delete e left, right]}
  | otherwise = Node {elm=elm, children=[left, delete e right]}



rotateRight :: Tree s -> Tree s
rotateRight (Node x [Node y [a, b], c]) = Node y [a, Node x [b, c]]
rotateRight node = node
