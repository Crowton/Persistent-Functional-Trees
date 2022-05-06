-- Code from: http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.55.5156&rep=rep1&type=pdf

module Random_access_list_persistent where

import DataRecords
import Prelude hiding (lookup)

import qualified Persistent_update as P
import qualified Persistent_query as Q


empty :: PartialTree (Int, a)
empty = P.construct_empty_tree 3


isempty' :: Tree (Int, a) -> Bool
isempty' Leaf = True
isempty' _ = False

isempty :: PartialTree (Int, a) -> Bool
isempty = Q.query (\_ -> isempty') Nothing


cons' :: Eq a => a -> UserTree (Int, a) -> Update (Int, a)
cons' x xs@(UserNode ((size1, elm1), con1, _, [(_, left1), (_, right1), (UserNode ((size2, elm2), con2, _, [(_, left2), (_, right2), (_, rest)]), _)])) =
    if size1 == size2
        then P.create_new_node (1 + size1 + size2, x) [con1 (size1, elm1) [left1, right1, P.leaf], con2 (size2, elm2) [left2, right2, P.leaf], rest]
        else P.create_new_node (1, x) [P.leaf, P.leaf, P.id_node xs]
cons' x xs = P.create_new_node (1, x) [P.leaf, P.leaf, P.id_node xs]

cons :: Eq a => a -> PartialTree (Int, a) -> PartialTree (Int, a)
cons = P.update cons'


head' :: Tree (Int, a) -> a
head' Leaf = error "Empty list"
head' (Node (_, x) _) = x

head :: PartialTree (Int, a) -> a
head = Q.query (\_ -> head') Nothing

tail' :: Eq a => UserTree (Int, a) -> Update (Int, a)
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
        then tree_lookup' i t
        else lookup' (i - size) next

lookup :: Int -> PartialTree (Int, a) -> a
lookup = Q.query lookup'


update' :: Eq a => (Int, a) -> UserTree (Int, a) -> Update (Int, a)
update' _ UserLeaf = error "Index out of bounds"
update' (i, y) t@(UserNode ((size, x), con, _, [(_, l), (_, r), (next, _)])) =
    if i < size
        then tree_update' (i, y) t
        else con (size, x) [l, r, update' (i - size, y) next]

update :: Eq a => (Int, a) -> PartialTree (Int, a) -> PartialTree (Int, a)
update = P.update update'


tree_lookup' :: Int -> Tree (Int, a) -> a
tree_lookup' _ Leaf = error "Index out of bounds"
tree_lookup' 0 (Node (_, x) _) = x
tree_lookup' i (Node (size, x) [left, right, _]) =
    let size' = size `div` 2 in
    if i <= size'
        then tree_lookup' (i - 1) left
        else tree_lookup' (i - 1 - size') right

tree_update' :: (Int, a) -> UserTree (Int, a) -> Update (Int, a)
tree_update' _ UserLeaf = error "Index out of bounds"
tree_update' (0, y) (UserNode ((size, _), con, _, c)) = con (size, y) (map snd c)
tree_update' (i, y) (UserNode ((size, x), con, _, [(left_tree, left_ret), (right_tree, right_ret), (_, c)])) =
    let size' = size `div` 2 in
    if i <= size'
        then con (size, x) [tree_update' (i - 1, y) left_tree, right_ret, c]
        else con (size, x) [left_ret, tree_update' (i - 1 - size', y) right_tree, c]
