{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Binary_Tree_persistent where

import DataRecords
import Persistent_update


get_func :: Ord s => PER_BST s s
get_func = (empty, insert, delete)


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


extract_max' :: Ord e => UserTree e  -> (Update e, Maybe e)
extract_max' UserLeaf = (leaf, Nothing)
extract_max' (UserNode (elm, _, rep, [(_, left_ret), (UserLeaf, _)])) = (rep left_ret, Just elm)
extract_max' (UserNode (elm, con, _, [(_, left_ret), (right_tree, _)])) =
    let (right_ret', max) = extract_max' right_tree in
    (con elm [left_ret, right_ret'], max)

delete' :: Ord e => e -> UserTree e -> Update e
delete' _ UserLeaf = leaf
delete' e (UserNode (elm, con, rep, [(left_tree, left_ret), (right_tree, right_ret)]))
    | e == elm = let (left_ret', max) = extract_max' left_tree in
                 case max of
                    Nothing -> rep right_ret
                    Just elm' -> con elm' [left_ret', right_ret]
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


rotate_right_left :: Eq e => UserTree e -> Update e
rotate_right_left (UserNode (elm, con, _, [(left_tree, _), (_, right_ret)])) =
    tree_to_update rotate_right (con elm [rotate_left left_tree, right_ret])
