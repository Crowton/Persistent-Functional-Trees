{-# LANGUAGE PatternSynonyms #-}

-- Code is from: https://hackage.haskell.org/package/llrbtree-0.1.1/docs/src/Data-Set-RBTree.html

module RBTreePersistent where

import DataRecords
import PersistentUpdate

import RBTreeEphemeral ( Color(..) )

----------------------------------------------------------------

getFunc :: Ord s => PER_BST s (Color, s)
getFunc = (empty, insert, delete)

-- data Color = B -- ^ Black
--            | R -- ^ Red
--            deriving (Eq, Show)

----------------------------------------------------------------

empty :: PartialTree (Color, a)
empty = constructEmptyTree 2

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
insert = update insertRoot

insertRoot :: Ord a => a -> UserTree (Color, a) -> TreeUpdate (Color, a)
insertRoot kx t = treeToUpdate turnB (insert' kx t)

insert' :: Ord a => a -> UserTree (Color, a) -> TreeUpdate (Color, a)
insert' kx UserLeaf = createNewNode (R, kx) [leaf, leaf]
insert' kx s@(UserNode ((B, x), con, _, [(lt, lr), (rt, rr)])) = case compare kx x of
    LT -> treeToUpdate balanceL (con (B, x) [(insert' kx lt), rr])
    GT -> treeToUpdate balanceR (con (B, x) [lr, (insert' kx rt)])
    EQ -> idNode s
insert' kx s@(UserNode ((R, x), con, _, [(lt, lr), (rt, rr)])) = case compare kx x of
    LT -> con (R, x) [(insert' kx lt), rr]
    GT -> con (R, x) [lr, (insert' kx rt)]
    EQ -> idNode s

----------------------------------------------------------------

balanceL :: UserTree (Color, a) -> TreeUpdate (Color, a)
balanceL (UserNode ((B, z), conZ, _, [(UserNode ((R, y), conY, _, [(UserNode ((R, x), _, repX, [(_, a), (_, b)]), _), (_, c)]), _), (_, d)])) =
    conZ (R, y) [conY (B, x) [repX a, b], createNewNode (B, z) [c, d]]
balanceL (UserNode ((B, z), conZ, _, [(UserNode ((R, x), conX, _, [(_, a), (UserNode ((R, y), _, repY, [(_, b), (_, c)]), _)]), _), (_, d)])) =
    conZ (R, y) [conX (B, x) [a, repY b], createNewNode (B, z) [c, d]]
balanceL node = idNode node

balanceR :: UserTree (Color, a) -> TreeUpdate (Color, a)
balanceR (UserNode ((B, x), conX, _, [(_, a), (UserNode ((R, y), conY, _, [(_, b), (UserNode ((R, z), _, repZ, [(_, c), (_, d)]), _)]), _)])) =
    conX (R, y) [createNewNode (B, x) [a, b], conY (B, z) [c, repZ d]]
balanceR (UserNode ((B, x), conX, _, [(_, a), (UserNode ((R, z), conZ, _, [(UserNode ((R, y), _, repY, [(_, b), (_, c)]), _), (_, d)]), _)])) =
    conX (R, y) [createNewNode (B, x) [a, b], conZ (B, z) [repY c, d]]
balanceR node = idNode node

----------------------------------------------------------------

unbalancedL :: Eq a => UserTree (Color, a) -> Update (Color, a) (TreeUpdate (Color, a), Bool)
unbalancedL (UserNode ((c, x), conX, _, [(l@(UserNode ((B, _), _, _, _)), _), (_, r)]))
  = idFunc (treeToUpdate balanceL (conX (B, x) [(turnR l), r]), c == B)
unbalancedL (UserNode ((B, x), conX, _, [(UserNode ((R, lx), _, repLx, [(_, ll), (lr@(UserNode ((B, _), _, _, _)), _)]), _), (_, r)]))
  = idFunc (conX (B, lx) [repLx ll, treeToUpdate balanceL (createNewNode (B, x) [turnR lr, r])], False)
unbalancedL _ = error "unbalancedL"

unbalancedR :: Eq a => UserTree (Color, a) -> Update (Color, a) (TreeUpdate (Color, a), Bool)
unbalancedR (UserNode ((c, x), conX, _, [(_, l), (r@(UserNode ((B, _), _, _, _)), _)]))
  = idFunc (treeToUpdate balanceR (conX (B, x) [l, turnR r]), c == B)
unbalancedR (UserNode ((B, x), conX, _, [(_, l), (UserNode ((R, rx), _, repRx, [(rl@(UserNode ((B, _), _, _, _)), _), (_, rr)]), _)]))
  = idFunc (conX (B, rx) [treeToUpdate balanceR (createNewNode (B, x) [l, turnR rl]), repRx rr], False)
unbalancedR _ = error "unbalancedR"

----------------------------------------------------------------

deleteMin' :: Ord a => UserTree (Color, a) -> Update (Color, a) (Update (Color, a) (TreeUpdate (Color, a), Bool), a)
deleteMin' UserLeaf
    = error "deleteMin'"
deleteMin' (UserNode ((B, x), _, rep, [(UserLeaf, _), (UserLeaf, _)]))
    = idFunc (idFunc (rep leaf, True), x)
deleteMin' (UserNode ((B, x), _, rep, [(UserLeaf, _), (rTree@(UserNode ((R, _), _, _, _)), _)]))
    = idFunc (idFunc (rep (turnB rTree), False), x)
deleteMin' (UserNode ((R, x), _, rep, [(UserLeaf, _), (_, rRet)]))
    = idFunc (idFunc (rep rRet, False), x)
deleteMin' (UserNode ((c, x), con, _, [(l, _), (_, rRet)]))
    = chainUpdate
        (\(ld, m) -> 
                chainUpdate
                    (\(l', d) ->
                        if d
                            then idFunc (treeToUpdate unbalancedR (con (c, x) [l', rRet]), m)
                            else idFunc (idFunc (con (c, x) [l', rRet], False), m)
                    ) ld
        ) (deleteMin' l)

----------------------------------------------------------------

blackify :: UserTree (Color, a) -> (TreeUpdate (Color, a), Bool)
blackify s@(UserNode ((R, _), _, _, _)) = (turnB s, False)
blackify s                              = (idNode s, True)

delete :: Ord a => a -> PartialTree (Color, a) -> PartialTree (Color, a)
delete = update deleteRoot

deleteRoot :: Ord a => a -> UserTree (Color, a) -> TreeUpdate (Color, a)
deleteRoot x t = chainUpdate (\(s, _) -> treeToUpdate turnB' s) (delete' x t)

delete' :: Ord a => a -> UserTree (Color, a) -> Update (Color, a) (TreeUpdate (Color, a), Bool)
delete' _ UserLeaf = idFunc (leaf, False)
delete' x (UserNode ((c, y), con, rep, [(lTree, lRet), (rTree, rRet)])) = case compare x y of
    LT -> chainUpdate
            (\(lRet', d) ->
                let t = con (c, y) [lRet', rRet] in 
                if d
                    then treeToUpdate unbalancedR t
                    else idFunc (t, False)
            ) (delete' x lTree)
    GT -> chainUpdate
            (\(rRet', d) ->
                let t = con (c, y) [lRet, rRet'] in 
                if d
                    then treeToUpdate unbalancedL t
                    else idFunc (t, False)
            ) (delete' x rTree)
    EQ -> case rTree of
        UserLeaf -> if c == B
                        then let (t, b) = blackify lTree in idFunc (rep t, b)
                        else idFunc (rep lRet, False)
        _ -> chainUpdate
                (\(rd, m) -> 
                    chainUpdate
                        (\(rRet', d) ->
                            let t = con (c, m) [lRet, rRet'] in 
                            if d
                                then treeToUpdate unbalancedL t
                                else idFunc (t, False)
                        ) rd
                ) (deleteMin' rTree)
