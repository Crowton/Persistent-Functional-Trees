{-# LANGUAGE PatternSynonyms #-}

-- Code is from: https://hackage.haskell.org/package/llrbtree-0.1.1/docs/src/Data-Set-RBTree.html

module RBTree_persistent where

import DataRecords
import Persistent_update

----------------------------------------------------------------

get_func :: Ord s => PER_BST s (Color, s)
get_func = (empty, insert, delete)

data Color = B -- ^ Black
           | R -- ^ Red
           deriving (Eq, Show)

----------------------------------------------------------------

empty :: PartialTree (Color, a)
empty = construct_empty_tree 2

----------------------------------------------------------------

member :: Ord a => a -> Tree (Color, a) -> Bool
member _ Leaf = False
member x (Node (_, y) [l, r]) = case compare x y of
    LT -> member x l
    GT -> member x r
    EQ -> True

----------------------------------------------------------------

turnR :: UserTree (Color, a) -> Update (Color, a)
turnR UserLeaf                                      = error "turnR"
turnR (UserNode ((_, x), con, _, [(_, l), (_, r)])) = con (R, x) [l, r]

turnB :: UserTree (Color, a) -> Update (Color, a)
turnB UserLeaf                                      = error "turnB"
turnB (UserNode ((_, x), con, _, [(_, l), (_, r)])) = con (B, x) [l, r]

turnB' :: UserTree (Color, a) -> Update (Color, a)
turnB' UserLeaf                                      = leaf
turnB' (UserNode ((_, x), con, _, [(_, l), (_, r)])) = con (B, x) [l, r]

----------------------------------------------------------------

insert :: Ord a => a -> PartialTree (Color, a) -> PartialTree (Color, a)
insert = update insert_root

insert_root :: Ord a => a -> UserTree (Color, a) -> Update (Color, a)
insert_root kx t = tree_to_update turnB (insert' kx t)

insert' :: Ord a => a -> UserTree (Color, a) -> Update (Color, a)
insert' kx UserLeaf = create_new_node (R, kx) [leaf, leaf]
insert' kx s@(UserNode ((B, x), con, _, [(lt, lr), (rt, rr)])) = case compare kx x of
    LT -> tree_to_update balanceL (con (B, x) [(insert' kx lt), rr])
    GT -> tree_to_update balanceR (con (B, x) [lr, (insert' kx rt)])
    EQ -> id_node s
insert' kx s@(UserNode ((R, x), con, _, [(lt, lr), (rt, rr)])) = case compare kx x of
    LT -> con (R, x) [(insert' kx lt), rr]
    GT -> con (R, x) [lr, (insert' kx rt)]
    EQ -> id_node s

----------------------------------------------------------------

balanceL :: UserTree (Color, a) -> Update (Color, a)
balanceL (UserNode ((B, z), con_z, _, [(UserNode ((R, y), con_y, _, [(UserNode ((R, x), _, rep_x, [(_, a), (_, b)]), _), (_, c)]), _), (_, d)])) =
    con_z (R, y) [con_y (B, x) [rep_x a, b], create_new_node (B, z) [c, d]]
balanceL (UserNode ((B, z), con_z, _, [(UserNode ((R, x), con_x, _, [(_, a), (UserNode ((R, y), _, rep_y, [(_, b), (_, c)]), _)]), _), (_, d)])) =
    con_z (R, y) [con_x (B, x) [a, rep_y b], create_new_node (B, z) [c, d]]
balanceL node = id_node node

balanceR :: UserTree (Color, a) -> Update (Color, a)
balanceR (UserNode ((B, x), con_x, _, [(_, a), (UserNode ((R, y), con_y, _, [(_, b), (UserNode ((R, z), _, rep_z, [(_, c), (_, d)]), _)]), _)])) =
    con_x (R, y) [create_new_node (B, x) [a, b], con_y (B, z) [c, rep_z d]]
balanceR (UserNode ((B, x), con_x, _, [(_, a), (UserNode ((R, z), con_z, _, [(UserNode ((R, y), _, rep_y, [(_, b), (_, c)]), _), (_, d)]), _)])) =
    con_x (R, y) [create_new_node (B, x) [a, b], con_z (B, z) [rep_y c, d]]
balanceR node = id_node node

----------------------------------------------------------------

-- unbalancedL :: Eq a => UserTree (Color, a) -> (Update (Color, a), Bool)
-- unbalancedL (UserNode ((c, x), con_x, _, [(l@(UserNode ((B, _), _, _, _)), _), (_, r)]))
--   = (tree_to_update balanceL (con_x (B, x) [(turnR l), r]), c == B)
-- unbalancedL (UserNode ((B, x), con_x, _, [(UserNode ((R, lx), _, rep_lx, [(_, ll), (lr@(UserNode ((B, _), _, _, _)), _)]), _), (_, r)]))
--   = (con_x (B, lx) [rep_lx ll, tree_to_update balanceL (create_new_node (B, x) [turnR lr, r])], False)
-- unbalancedL _ = error "unbalancedL"

-- unbalancedR :: Eq a => UserTree (Color, a) -> (Update (Color, a), Bool)
-- unbalancedR (UserNode ((c, x), con_x, _, [(_, l), (r@(UserNode ((B, _), _, _, _)), _)]))
--   = (tree_to_update balanceR (con_x (B, x) [l, turnR r]), c == B)
-- unbalancedR (UserNode ((B, x), con_x, _, [(_, l), (UserNode ((R, rx), _, rep_rx, [(rl@(UserNode ((B, _), _, _, _)), _), (_, rr)]), _)]))
--   = (con_x (B, rx) [tree_to_update balanceR (create_new_node (B, x) [l, turnR rl]), rep_rx rr], False)
-- unbalancedR _ = error "unbalancedR"

unbalancedL_tree :: Eq a => UserTree (Color, a) -> Update (Color, a)
unbalancedL_tree (UserNode ((c, x), con_x, _, [(l@(UserNode ((B, _), _, _, _)), _), (_, r)]))
  = tree_to_update balanceL (con_x (B, x) [(turnR l), r])
unbalancedL_tree (UserNode ((B, x), con_x, _, [(UserNode ((R, lx), _, rep_lx, [(_, ll), (lr@(UserNode ((B, _), _, _, _)), _)]), _), (_, r)]))
  = con_x (B, lx) [rep_lx ll, tree_to_update balanceL (create_new_node (B, x) [turnR lr, r])]
unbalancedL_tree _ = error "unbalancedL_tree"

unbalancedL_bool :: Eq a => UserTree (Color, a) -> Bool
unbalancedL_bool (UserNode ((c, x), con_x, _, [(l@(UserNode ((B, _), _, _, _)), _), (_, r)]))
  = c == B
unbalancedL_bool (UserNode ((B, x), con_x, _, [(UserNode ((R, lx), _, rep_lx, [(_, ll), (lr@(UserNode ((B, _), _, _, _)), _)]), _), (_, r)]))
  = False
unbalancedL_bool _ = error "unbalancedL_bool"

unbalancedR_tree :: Eq a => UserTree (Color, a) -> Update (Color, a)
unbalancedR_tree (UserNode ((c, x), con_x, _, [(_, l), (r@(UserNode ((B, _), _, _, _)), _)]))
  = tree_to_update balanceR (con_x (B, x) [l, turnR r])
unbalancedR_tree (UserNode ((B, x), con_x, _, [(_, l), (UserNode ((R, rx), _, rep_rx, [(rl@(UserNode ((B, _), _, _, _)), _), (_, rr)]), _)]))
  = con_x (B, rx) [tree_to_update balanceR (create_new_node (B, x) [l, turnR rl]), rep_rx rr]
unbalancedR_tree _ = error "unbalancedR_tree"

unbalancedR_bool :: Eq a => UserTree (Color, a) -> Bool
unbalancedR_bool (UserNode ((c, x), con_x, _, [(_, l), (r@(UserNode ((B, _), _, _, _)), _)]))
  = c == B
unbalancedR_bool (UserNode ((B, x), con_x, _, [(_, l), (UserNode ((R, rx), _, rep_rx, [(rl@(UserNode ((B, _), _, _, _)), _), (_, rr)]), _)]))
  = False
unbalancedR_bool _ = error "unbalancedR_bool"

----------------------------------------------------------------

deleteMin' :: UserTree (Color, a) -> ((Update (Color, a), Bool), a)
deleteMin' UserLeaf
    = error "deleteMin'"
deleteMin' (UserNode ((B, x), _, rep, [(UserLeaf, _), (UserLeaf, _)]))
    = ((rep leaf, True), x)
deleteMin' (UserNode ((B, x), _, rep, [(UserLeaf, _), (r_tree, _)]))
    = ((rep (turnB' r_tree), False), x)
deleteMin' (UserNode ((c, x), con, _, [(l, _), (_, r_ret)]))
    = if d then (tD, m) else (tD', m)
  where
    ((l', d), m) = deleteMin' l
    tD  = tree_to_update unbalancedR_tree (con (c, x) [l', r_ret])    -- TODO: issues with type update stuff, need new func?????
    tD' = (con (c, x) [l', r_ret], False)

----------------------------------------------------------------

blackify :: UserTree (Color, a) -> (Update (Color, a), Bool)
blackify s@(UserNode ((R, _), _, _, _)) = (turnB s, False)
blackify s                              = (id_node s, True)

delete :: Ord a => a -> PartialTree (Color, a) -> PartialTree (Color, a)
delete = update delete_root

delete_root :: Ord a => a -> UserTree (Color, a) -> Update (Color, a)
delete_root x t = tree_to_update turnB' s
  where
    (s, _) = delete' x t

delete' :: Ord a => a -> UserTree (Color, a) -> (Update (Color, a), Bool)
delete' _ UserLeaf = (leaf, False)
delete' x (UserNode ((c, y), con, rep, [(l_tree, l_ret), (r_tree, r_ret)])) = case compare x y of
    LT -> let (l_ret', d) = delete' x l_tree
              t = con (c, y) [l_ret', r_ret]
          in if d then tree_to_update unbalancedR_tree t else (t, False)
    GT -> let (r_ret', d) = delete' x r_tree
              t = con (c, y) [l_ret, r_ret']
          in if d then tree_to_update unbalancedL_tree t else (t, False)
    EQ -> case r_tree of
        UserLeaf -> if c == B
                        then let (t, b) = blackify l_tree in (rep t, b)
                        else (rep l_ret, False)
        _ -> let ((r_ret', d), m) = deleteMin' r_tree
                 t = con (c, m) [l_ret, r_ret']
             in if d then tree_to_update unbalancedL_tree t else (t, False)
