{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

-- Code is from: https://hackage.haskell.org/package/llrbtree-0.1.1/docs/src/Data-Set-RBTree.html

module RBTreeEphemeral where

import DataRecords
import GHC.Generics (Generic)
import Control.DeepSeq

import Prelude hiding (sum)

----------------------------------------------------------------

getFunc :: Ord s => EPH_BST s (Color, s)
getFunc = (empty, insert, delete)

data Color = B -- ^ Black
           | R -- ^ Red
           deriving (Eq, Show, Generic, NFData)

-- Syntactic help
type RBTree a = Tree (Color, a)
pattern RBNode c l x r = Node (c, x) [l, r]

----------------------------------------------------------------

empty :: RBTree a
empty = Leaf

----------------------------------------------------------------

member :: Ord a => a -> RBTree a -> Bool
member _ Leaf = False
member x (RBNode _ l y r) = case compare x y of
    LT -> member x l
    GT -> member x r
    EQ -> True

sum :: Tree (Color, Int) -> Int
sum Leaf = 0
sum Node {elm=(_, elm), children=[left, right]} = (sum left) + elm + (sum right)

----------------------------------------------------------------

turnR :: RBTree a -> RBTree a
turnR Leaf             = error "turnR"
turnR (RBNode _ l x r) = RBNode R l x r

turnB :: RBTree a -> RBTree a
turnB Leaf             = error "turnB"
turnB (RBNode _ l x r) = RBNode B l x r

turnB' :: RBTree a -> RBTree a
turnB' Leaf             = Leaf
turnB' (RBNode _ l x r) = RBNode B l x r

----------------------------------------------------------------

insert :: Ord a => a -> RBTree a -> RBTree a
insert kx t = turnB (insert' kx t)

insert' :: Ord a => a -> RBTree a -> RBTree a
insert' kx Leaf = RBNode R Leaf kx Leaf
insert' kx s@(RBNode B l x r) = case compare kx x of
    LT -> balanceL B (insert' kx l) x r
    GT -> balanceR B l x (insert' kx r)
    EQ -> s
insert' kx s@(RBNode R l x r) = case compare kx x of
    LT -> RBNode R (insert' kx l) x r
    GT -> RBNode R l x (insert' kx r)
    EQ -> s

----------------------------------------------------------------

balanceL :: Color -> RBTree a -> a -> RBTree a -> RBTree a
balanceL B (RBNode R (RBNode R a x b) y c) z d =
    RBNode R (RBNode B a x b) y (RBNode B c z d)
balanceL B (RBNode R a x (RBNode R b y c)) z d =
    RBNode R (RBNode B a x b) y (RBNode B c z d)
balanceL k l x r = RBNode k l x r

balanceR :: Color -> RBTree a -> a -> RBTree a -> RBTree a
balanceR B a x (RBNode R b y (RBNode R c z d)) =
    RBNode R (RBNode B a x b) y (RBNode B c z d)
balanceR B a x (RBNode R (RBNode R b y c) z d) =
    RBNode R (RBNode B a x b) y (RBNode B c z d)
balanceR k l x r = RBNode k l x r

----------------------------------------------------------------

type RBTreeBDel a = (RBTree a, Bool)

unbalancedL :: Color -> RBTree a -> a -> RBTree a -> RBTreeBDel a
unbalancedL c l@(RBNode B _ _ _) x r
  = (balanceL B (turnR l) x r, c == B)
unbalancedL B (RBNode R ll lx lr@(RBNode B _ _ _)) x r
  = (RBNode B ll lx (balanceL B (turnR lr) x r), False)
unbalancedL _ _ _ _ = error "unbalancedL"

-- The left tree lacks one Black node
unbalancedR :: Color -> RBTree a -> a -> RBTree a -> (RBTree a, Bool)
-- Decreasing one Black node in the right
unbalancedR c l x r@(RBNode B _ _ _)
  = (balanceR B l x (turnR r), c == B)
-- Taking one Red node from the right and adding it to the right as Black
unbalancedR B l x (RBNode R rl@(RBNode B _ _ _) rx rr)
  = (RBNode B (balanceR B l x (turnR rl)) rx rr, False)
unbalancedR _ _ _ _ = error "unbalancedR"

----------------------------------------------------------------

deleteMin' :: RBTree a -> (RBTreeBDel a, a)
deleteMin' Leaf                                 = error "deleteMin'"
deleteMin' (RBNode B Leaf x Leaf)               = ((Leaf, True), x)
deleteMin' (RBNode B Leaf x r@(RBNode R _ _ _)) = ((turnB r, False), x)
deleteMin' (RBNode R Leaf x r)                  = ((r, False), x)
deleteMin' (RBNode c l x r)                     = if d then (tD, m) else (tD', m)
  where
    ((l', d), m) = deleteMin' l
    tD  = unbalancedR c l' x r
    tD' = (RBNode c l' x r, False)

----------------------------------------------------------------

blackify :: RBTree a -> RBTreeBDel a
blackify s@(RBNode R _ _ _) = (turnB s, False)
blackify s                  = (s, True)

delete :: Ord a => a -> RBTree a -> RBTree a
delete x t = turnB' s
  where
    (s, _) = delete' x t

delete' :: Ord a => a -> RBTree a -> RBTreeBDel a
delete' _ Leaf = (Leaf, False)
delete' x (RBNode c l y r) = case compare x y of
    LT -> let (l', d) = delete' x l
              t = RBNode c l' y r
          in if d then unbalancedR c l' y r else (t, False)
    GT -> let (r', d) = delete' x r
              t = RBNode c l y r'
          in if d then unbalancedL c l y r' else (t, False)
    EQ -> case r of
        Leaf -> if c == B then blackify l else (l, False)
        _ -> let ((r', d), m) = deleteMin' r
                 t = RBNode c l m r'
             in if d then unbalancedL c l m r' else (t, False)
