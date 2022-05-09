{-# LANGUAGE PatternSynonyms #-}

-- Code is from: https://hackage.haskell.org/package/llrbtree-0.1.1/docs/src/Data-Set-RBTree.html

module RBTree_persistent where

import DataRecords
import Persistent_update

import RBTree_temporal ( Color(..) )

----------------------------------------------------------------

get_func :: Ord s => PER_BST s (Color, s)
get_func = (empty, insert, delete)

-- data Color = B -- ^ Black
--            | R -- ^ Red
--            deriving (Eq, Show)

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

turnR :: UserTree (Color, a) -> TreeUpdate (Color, a)
turnR UserLeaf                                      = error "turnR"
turnR (UserNode ((_, x), con, _, [(_, l), (_, r)])) = con (R, x) [l, r]

turnB :: UserTree (Color, a) -> TreeUpdate (Color, a)
turnB UserLeaf                                      = error "turnB"
turnB (UserNode ((_, x), con, _, [(_, l), (_, r)])) = con (B, x) [l, r]

turnB' :: UserTree (Color, a) -> TreeUpdate (Color, a)
turnB' UserLeaf                                      = leaf
turnB' (UserNode ((_, x), con, _, [(_, l), (_, r)])) = con (B, x) [l, r]

----------------------------------------------------------------

insert :: Ord a => a -> PartialTree (Color, a) -> PartialTree (Color, a)
insert = update insert_root

insert_root :: Ord a => a -> UserTree (Color, a) -> TreeUpdate (Color, a)
insert_root kx t = tree_to_update turnB (insert' kx t)

insert' :: Ord a => a -> UserTree (Color, a) -> TreeUpdate (Color, a)
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

balanceL :: UserTree (Color, a) -> TreeUpdate (Color, a)
balanceL (UserNode ((B, z), con_z, _, [(UserNode ((R, y), con_y, _, [(UserNode ((R, x), _, rep_x, [(_, a), (_, b)]), _), (_, c)]), _), (_, d)])) =
    con_z (R, y) [con_y (B, x) [rep_x a, b], create_new_node (B, z) [c, d]]
balanceL (UserNode ((B, z), con_z, _, [(UserNode ((R, x), con_x, _, [(_, a), (UserNode ((R, y), _, rep_y, [(_, b), (_, c)]), _)]), _), (_, d)])) =
    con_z (R, y) [con_x (B, x) [a, rep_y b], create_new_node (B, z) [c, d]]
balanceL node = id_node node

balanceR :: UserTree (Color, a) -> TreeUpdate (Color, a)
balanceR (UserNode ((B, x), con_x, _, [(_, a), (UserNode ((R, y), con_y, _, [(_, b), (UserNode ((R, z), _, rep_z, [(_, c), (_, d)]), _)]), _)])) =
    con_x (R, y) [create_new_node (B, x) [a, b], con_y (B, z) [c, rep_z d]]
balanceR (UserNode ((B, x), con_x, _, [(_, a), (UserNode ((R, z), con_z, _, [(UserNode ((R, y), _, rep_y, [(_, b), (_, c)]), _), (_, d)]), _)])) =
    con_x (R, y) [create_new_node (B, x) [a, b], con_z (B, z) [rep_y c, d]]
balanceR node = id_node node

----------------------------------------------------------------

unbalancedL :: Eq a => UserTree (Color, a) -> Update (Color, a) (TreeUpdate (Color, a), Bool)
unbalancedL (UserNode ((c, x), con_x, _, [(l@(UserNode ((B, _), _, _, _)), _), (_, r)]))
  = id_func (tree_to_update balanceL (con_x (B, x) [(turnR l), r]), c == B)
unbalancedL (UserNode ((B, x), con_x, _, [(UserNode ((R, lx), _, rep_lx, [(_, ll), (lr@(UserNode ((B, _), _, _, _)), _)]), _), (_, r)]))
  = id_func (con_x (B, lx) [rep_lx ll, tree_to_update balanceL (create_new_node (B, x) [turnR lr, r])], False)
unbalancedL _ = error "unbalancedL"

unbalancedR :: Eq a => UserTree (Color, a) -> Update (Color, a) (TreeUpdate (Color, a), Bool)
unbalancedR (UserNode ((c, x), con_x, _, [(_, l), (r@(UserNode ((B, _), _, _, _)), _)]))
  = id_func (tree_to_update balanceR (con_x (B, x) [l, turnR r]), c == B)
unbalancedR (UserNode ((B, x), con_x, _, [(_, l), (UserNode ((R, rx), _, rep_rx, [(rl@(UserNode ((B, _), _, _, _)), _), (_, rr)]), _)]))
  = id_func (con_x (B, rx) [tree_to_update balanceR (create_new_node (B, x) [l, turnR rl]), rep_rx rr], False)
unbalancedR _ = error "unbalancedR"

----------------------------------------------------------------

-- TODO: remove update layers?
deleteMin' :: Ord a => UserTree (Color, a) -> Update (Color, a) (Update (Color, a) (TreeUpdate (Color, a), Bool), a)
deleteMin' UserLeaf
    = error "deleteMin'"
deleteMin' (UserNode ((B, x), _, rep, [(UserLeaf, _), (UserLeaf, _)]))
    = id_func (id_func (rep leaf, True), x)

deleteMin' (UserNode ((B, x), _, rep, [(UserLeaf, _), (r_tree, _)]))
    = id_func (id_func (rep (turnB' r_tree), False), x)

deleteMin' (UserNode ((B, x), _, rep, [(UserLeaf, _), (r_tree@(UserNode ((R, _), _, _, _)), _)]))
    = id_func (id_func (rep (turnB r_tree), False), x)
deleteMin' (UserNode ((R, x), _, rep, [(UserLeaf, _), (_, r_ret)]))
    = id_func (id_func (rep r_ret, False), x)

deleteMin' (UserNode ((c, x), con, _, [(l, _), (_, r_ret)]))
    = chain_update
        (\(ld, m) -> 
                chain_update
                    (\(l', d) ->
                        if d
                            then id_func (tree_to_update unbalancedR (con (c, x) [l', r_ret]), m)
                            else id_func (id_func (con (c, x) [l', r_ret], False), m)
                    ) ld
        ) (deleteMin' l)

----------------------------------------------------------------

blackify :: UserTree (Color, a) -> (TreeUpdate (Color, a), Bool)
blackify s@(UserNode ((R, _), _, _, _)) = (turnB s, False)
blackify s                              = (id_node s, True)

delete :: Ord a => a -> PartialTree (Color, a) -> PartialTree (Color, a)
delete = update delete_root

delete_root :: Ord a => a -> UserTree (Color, a) -> TreeUpdate (Color, a)
delete_root x t = chain_update (\(s, _) -> tree_to_update turnB' s) (delete' x t)

delete' :: Ord a => a -> UserTree (Color, a) -> Update (Color, a) (TreeUpdate (Color, a), Bool)
delete' _ UserLeaf = id_func (leaf, False)
delete' x (UserNode ((c, y), con, rep, [(l_tree, l_ret), (r_tree, r_ret)])) = case compare x y of
    LT -> chain_update
            (\(l_ret', d) ->
                let t = con (c, y) [l_ret', r_ret] in 
                if d
                    then tree_to_update unbalancedR t
                    else id_func (t, False)
            ) (delete' x l_tree)
    GT -> chain_update
            (\(r_ret', d) ->
                let t = con (c, y) [l_ret, r_ret'] in 
                if d
                    then tree_to_update unbalancedL t
                    else id_func (t, False)
            ) (delete' x r_tree)
    EQ -> case r_tree of
        UserLeaf -> if c == B
                        then let (t, b) = blackify l_tree in id_func (rep t, b)
                        else id_func (rep l_ret, False)
        _ -> chain_update
                (\(rd, m) -> 
                    chain_update
                        (\(r_ret', d) ->
                            let t = con (c, m) [l_ret, r_ret'] in 
                            if d
                                then tree_to_update unbalancedL t
                                else id_func (t, False)
                        ) rd
                ) (deleteMin' r_tree)
