{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Binary_Tree_persistent where

import DataRecords
import Persistent_update

empty :: PartialTree s
empty = construct_empty_tree 2

insert' :: Ord s => s -> UserTree s -> Update s
insert' e UserLeaf = create_new_node e [leaf, leaf]
insert' e (UserNode (elm, node, rep_node, [(left_tree, left_ret), (right_tree, right_ret)]))
    | e == elm = node [left_ret, right_ret]
    | e < elm = node [insert' e left_tree, right_ret]
    | otherwise = node [left_ret, insert' e right_tree]

insert :: Ord s => s -> PartialTree s -> PartialTree s
insert = update insert'
