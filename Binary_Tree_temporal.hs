{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Binary_Tree_temporal where

import DataRecords


get_func :: Ord s => TEM_BST s
get_func = (Leaf, insert, delete)


contains :: Ord e => e -> Tree e -> Bool
contains _ Leaf = False
contains e Node {elm=elm, children=[left, right]}
  | e == elm = True
  | e < elm = contains e left
  | otherwise = contains e right


insert :: Ord e => e -> Tree e -> Tree e
insert e Leaf = Node {elm=e, children=[Leaf, Leaf]}
insert e Node {elm=elm, children=[left, right]}
  | e == elm = Node {elm=elm, children=[left, right]}
  | e < elm = Node {elm=elm, children=[insert e left, right]}
  | otherwise = Node {elm=elm, children=[left, insert e right]}


extract_max :: Ord e => Tree e -> (Tree e, Maybe e)
extract_max Leaf = (Leaf, Nothing)
extract_max Node {elm=elm, children=[left, Leaf]} = (left, Just elm)
extract_max Node {elm=elm, children=[left, right]} =
    let (right', max) = extract_max right in
    (Node {elm=elm, children=[left, right']}, max)

delete :: Ord e => e -> Tree e -> Tree e
delete _ Leaf = Leaf
delete e Node {elm=elm, children=[left, right]}
  | e == elm = let (left', max) = extract_max left in
               case max of
                   Nothing -> right
                   Just elm' -> Node {elm=elm', children=[left', right]}
  | e < elm = Node {elm=elm, children=[delete e left, right]}
  | otherwise = Node {elm=elm, children=[left, delete e right]}



rotate_right :: Tree s -> Tree s
rotate_right (Node x [Node y [a, b], c]) = Node y [a, Node x [b, c]]
rotate_right node = node
