{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Binary_Tree_persistent where

import DataRecords
import Persistent_update
import Persistent_query

empty :: PartialTree s
empty = construct_empty_tree 2


insert' :: Ord e => e -> UserTree e -> Update e
insert' e UserLeaf = create_new_node e [leaf, leaf]
insert' e (UserNode (elm, node, rep_node, _, [(left_tree, left_ret), (right_tree, right_ret)]))
    | e == elm = node [left_ret, right_ret]
    | e < elm = node [insert' e left_tree, right_ret]
    | otherwise = node [left_ret, insert' e right_tree]

insert :: Ord e => e -> PartialTree e -> PartialTree e
insert = update insert'


get_max' :: Ord e => UserTree e -> Maybe e
get_max' UserLeaf = Nothing
get_max' (UserNode (elm, _, _, _, [_, (UserLeaf, _)])) = Just elm
get_max' (UserNode (_, _, _, _, [_, (right_tree, _)])) = get_max' right_tree

delete_max' :: UserTree e -> Update e
delete_max' UserLeaf = leaf
delete_max' (UserNode (elm, _, _, overwrite_node, [(_, left_ret), (UserLeaf, _)])) = overwrite_node left_ret
delete_max' (UserNode (elm, node, _, _, [(left_tree, left_ret), (right_tree, right_ret)])) =
    node [left_ret, delete_max' right_tree]

delete' :: Ord e => e -> UserTree e -> Update e
delete' _ UserLeaf = leaf
delete' e (UserNode (elm, node, rep_node, overwrite_node, [(left_tree, left_ret), (right_tree, right_ret)]))
  | e == elm = case get_max' left_tree of
                   Nothing -> overwrite_node right_ret
                   Just elm' -> rep_node elm' [delete_max' left_tree, right_ret]
  | e < elm = node [delete' e left_tree, right_ret]
  | otherwise = node [left_ret, delete' e right_tree]

delete :: Ord e => e -> PartialTree e -> PartialTree e
delete = update delete'
