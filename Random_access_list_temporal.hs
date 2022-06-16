-- Code from: http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.55.5156

module Random_access_list_temporal where

import DataRecords
import Prelude hiding (lookup)


type RAL a = Tree (Int, a)


empty :: RAL a
empty = Leaf


isempty :: RAL a -> Bool
isempty Leaf = True
isempty _ = False


cons :: a -> RAL a -> RAL a
cons x xs@(Node (size1, elm1) [left1, right1, Node (size2, elm2) [left2, right2, rest]]) =
    if size1 == size2
        then Node (1 + size1 + size2, x) [Node (size1, elm1) [left1, right1, Leaf], Node (size2, elm2) [left2, right2, Leaf], rest]
        else Node (1, x) [Leaf, Leaf, xs]
cons x xs = Node (1, x) [Leaf, Leaf, xs]

head :: RAL a -> a
head Leaf = error "Empty list"
head (Node (_, x) _) = x

tail :: RAL a -> RAL a
tail Leaf = error "Empty list"
tail (Node _ [Leaf, Leaf, rest]) = rest
tail (Node _ [Node (size1, elm1) [left1, right1, Leaf], Node (size2, elm2) [left2, right2, Leaf], rest]) =
        Node (size1, elm1) [left1, right1, Node (size2, elm2) [left2, right2, rest]]


lookup :: Int -> RAL a -> a
lookup _ Leaf = error "Index out of bounds"
lookup i t@(Node (size, _) [_, _, next]) =
    if i < size
        then tree_lookup i t
        else lookup (i - size) next

update :: (Int, a) -> RAL a -> RAL a
update _ Leaf = error "Index out of bounds"
update (i, y) t@(Node (size, x) [l, r, next]) =
    if i < size
        then tree_update (i, y) t
        else Node (size, x) [l, r, update (i - size, y) next]


tree_lookup :: Int -> RAL a -> a
tree_lookup _ Leaf = error "Index out of bounds"
tree_lookup 0 (Node (_, x) _) = x
tree_lookup i (Node (size, x) [left, right, _]) =
    let size' = size `div` 2 in
    if i <= size'
        then tree_lookup (i - 1) left
        else tree_lookup (i - 1 - size') right

tree_update :: (Int, a) -> RAL a -> RAL a
tree_update _ Leaf = error "Index out of bounds"
tree_update (0, y) (Node (size, _) c) = Node (size, y) c
tree_update (i, y) (Node (size, x) [left, right, c]) =
    let size' = size `div` 2 in
    if i <= size'
        then Node (size, x) [tree_update (i - 1, y) left, right, c]
        else Node (size, x) [left, tree_update (i - 1 - size', y) right, c]
