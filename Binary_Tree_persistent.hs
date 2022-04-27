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
insert' e (UserNode (elm, con, _, [(left_tree, left_ret), (right_tree, right_ret)]))
    | e == elm = con elm [left_ret, right_ret]
    | e < elm = con elm [insert' e left_tree, right_ret]
    | otherwise = con elm [left_ret, insert' e right_tree]

insert :: Ord e => e -> PartialTree e -> PartialTree e
insert = update insert'


get_max' :: Ord e => UserTree e -> Maybe e
get_max' UserLeaf = Nothing
get_max' (UserNode (elm, _, _, [_, (UserLeaf, _)])) = Just elm
get_max' (UserNode (_, _, _, [_, (right_tree, _)])) = get_max' right_tree

delete_max' :: UserTree e -> Update e
delete_max' UserLeaf = leaf
delete_max' (UserNode (elm, _, rep, [(_, left_ret), (UserLeaf, _)])) = rep left_ret
delete_max' (UserNode (elm, con, _, [(left_tree, left_ret), (right_tree, right_ret)])) =
    con elm [left_ret, delete_max' right_tree]

delete' :: Ord e => e -> UserTree e -> Update e
delete' _ UserLeaf = leaf
delete' e (UserNode (elm, con, rep, [(left_tree, left_ret), (right_tree, right_ret)]))
  | e == elm = case get_max' left_tree of
                   Nothing -> rep right_ret
                   Just elm' -> con elm' [delete_max' left_tree, right_ret]
  | e < elm = con elm [delete' e left_tree, right_ret]
  | otherwise = con elm [left_ret, delete' e right_tree]

delete :: Ord e => e -> PartialTree e -> PartialTree e
delete = update delete'


rotate_right :: UserTree e -> Update e
rotate_right (UserNode (x, con_x, _, [(UserNode (y, _, rep_y, [(_, a), (_, b)]), _), (_, c)]))
    = con_x y [rep_y a, create_new_node x [b, c]]
rotate_right node = id_node node

rotate_left :: UserTree e -> Update e
rotate_left (UserNode (x, con_x, _, [(_, a), (UserNode (y, _, rep_y, [(_, b), (_, c)]), _)]))
    = con_x y [create_new_node x [a, b], rep_y c]
rotate_left node = id_node node


rotate_right_left :: UserTree e -> Update e
rotate_right_left (UserNode (elm, con, _, [(left_tree, _), (_, right_ret)])) =
    rotate_right (con elm [rotate_left left_tree, right_ret])
