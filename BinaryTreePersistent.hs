{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module BinaryTreePersistent where

import DataRecords
import PersistentUpdate


getFunc :: Ord s => PER_BST s s
getFunc = (empty, insert, delete)


empty :: PartialTree s
empty = constructEmptyTree 2


insert' :: Ord e => e -> UserTree e -> TreeUpdate e
insert' e UserLeaf = createNewNode e [leaf, leaf]
insert' e (UserNode (elm, con, _, [(leftTree, leftRet), (rightTree, rightRet)]))
    | e == elm = con elm [leftRet, rightRet]
    | e < elm = con elm [insert' e leftTree, rightRet]
    | otherwise = con elm [leftRet, insert' e rightTree]

insert :: Ord e => e -> PartialTree e -> PartialTree e
insert = update insert'


extractMax' :: Ord e => UserTree e  -> (TreeUpdate e, Maybe e)
extractMax' UserLeaf = (leaf, Nothing)
extractMax' (UserNode (elm, _, rep, [(_, leftRet), (UserLeaf, _)])) = (rep leftRet, Just elm)
extractMax' (UserNode (elm, con, _, [(_, leftRet), (rightTree, _)])) =
    let (rightRet', max) = extractMax' rightTree in
    (con elm [leftRet, rightRet'], max)

delete' :: Ord e => e -> UserTree e -> TreeUpdate e
delete' _ UserLeaf = leaf
delete' e (UserNode (elm, con, rep, [(leftTree, leftRet), (rightTree, rightRet)]))
    | e == elm = let (leftRet', max) = extractMax' leftTree in
                 case max of
                    Nothing -> rep rightRet
                    Just elm' -> con elm' [leftRet', rightRet]
    | e < elm = con elm [delete' e leftTree, rightRet]
    | otherwise = con elm [leftRet, delete' e rightTree]

delete :: Ord e => e -> PartialTree e -> PartialTree e
delete = update delete'




rotateRight :: UserTree e -> TreeUpdate e
rotateRight (UserNode (x, conX, _, [(UserNode (y, _, repY, [(_, a), (_, b)]), _), (_, c)]))
    = conX y [repY a, createNewNode x [b, c]]
rotateRight node = idNode node

rotateLeft :: UserTree e -> TreeUpdate e
rotateLeft (UserNode (x, conX, _, [(_, a), (UserNode (y, _, repY, [(_, b), (_, c)]), _)]))
    = conX y [createNewNode x [a, b], repY c]
rotateLeft node = idNode node


rotateRightLeft :: Eq e => UserTree e -> TreeUpdate e
rotateRightLeft (UserNode (elm, con, _, [(leftTree, _), (_, rightRet)])) =
    treeToUpdate rotateRight (con elm [rotateLeft leftTree, rightRet])
