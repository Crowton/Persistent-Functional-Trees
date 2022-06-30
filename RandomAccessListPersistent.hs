-- Code from: http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.55.5156

module RandomAccessListPersistent where

import DataRecords
import Prelude hiding (lookup)

import qualified PersistentUpdate as P
import qualified PersistentQuery as Q


empty :: PartialTree (Int, a)
empty = P.constructEmptyTree 3


isempty' :: Tree (Int, a) -> Bool
isempty' Leaf = True
isempty' _ = False

isempty :: PartialTree (Int, a) -> Bool
isempty = Q.query (\_ -> isempty') Nothing


cons' :: Eq a => a -> UserTree (Int, a) -> TreeUpdate (Int, a)
cons' x xs@(UserNode ((size1, elm1), con1, _, [(_, left1), (_, right1), (UserNode ((size2, elm2), con2, _, [(_, left2), (_, right2), (_, rest)]), _)])) =
    if size1 == size2
        then P.createNewNode (1 + size1 + size2, x) [con1 (size1, elm1) [left1, right1, P.leaf], con2 (size2, elm2) [left2, right2, P.leaf], rest]
        else P.createNewNode (1, x) [P.leaf, P.leaf, P.idNode xs]
cons' x xs = P.createNewNode (1, x) [P.leaf, P.leaf, P.idNode xs]

cons :: Eq a => a -> PartialTree (Int, a) -> PartialTree (Int, a)
cons = P.update cons'


head' :: Tree (Int, a) -> a
head' Leaf = error "Empty list"
head' (Node (_, x) _) = x

head :: PartialTree (Int, a) -> a
head = Q.query (\_ -> head') Nothing

tail' :: Eq a => UserTree (Int, a) -> TreeUpdate (Int, a)
tail' UserLeaf = error "Empty list"
tail' (UserNode (_, _, rep, [(UserLeaf, _), (UserLeaf, _), (_, rest)])) = rep rest
tail' (UserNode (_, _, rep, [(UserNode (val1, con1, _, [(_, left1), (_, right1), (UserLeaf, _)]), _), (UserNode (val2, con2, _, [(_, left2), (_, right2), (UserLeaf, _)]), _), (_, rest)])) =
        rep (con1 val1 [left1, right1, con2 val2 [left2, right2, rest]])

tail :: Eq a => PartialTree (Int, a) -> PartialTree (Int, a)
tail = P.update (\_ -> tail') Nothing


lookup' :: Int -> Tree (Int, a) -> a
lookup' _ Leaf = error "Index out of bounds"
lookup' i t@(Node (size, _) [_, _, next]) =
    if i < size
        then treeLookup' i t
        else lookup' (i - size) next

lookup :: Int -> PartialTree (Int, a) -> a
lookup = Q.query lookup'


update' :: Eq a => (Int, a) -> UserTree (Int, a) -> TreeUpdate (Int, a)
update' _ UserLeaf = error "Index out of bounds"
update' (i, y) t@(UserNode ((size, x), con, _, [(_, l), (_, r), (next, _)])) =
    if i < size
        then treeUpdate' (i, y) t
        else con (size, x) [l, r, update' (i - size, y) next]

update :: Eq a => (Int, a) -> PartialTree (Int, a) -> PartialTree (Int, a)
update = P.update update'


treeLookup' :: Int -> Tree (Int, a) -> a
treeLookup' _ Leaf = error "Index out of bounds"
treeLookup' 0 (Node (_, x) _) = x
treeLookup' i (Node (size, x) [left, right, _]) =
    let size' = size `div` 2 in
    if i <= size'
        then treeLookup' (i - 1) left
        else treeLookup' (i - 1 - size') right

treeUpdate' :: (Int, a) -> UserTree (Int, a) -> TreeUpdate (Int, a)
treeUpdate' _ UserLeaf = error "Index out of bounds"
treeUpdate' (0, y) (UserNode ((size, _), con, _, c)) = con (size, y) (map snd c)
treeUpdate' (i, y) (UserNode ((size, x), con, _, [(leftTree, leftRet), (rightTree, rightRet), (_, c)])) =
    let size' = size `div` 2 in
    if i <= size'
        then con (size, x) [treeUpdate' (i - 1, y) leftTree, rightRet, c]
        else con (size, x) [leftRet, treeUpdate' (i - 1 - size', y) rightTree, c]
