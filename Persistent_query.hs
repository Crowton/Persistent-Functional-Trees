{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Persistent_query where

import DataRecords


time_tree_to_tree :: TimeTree s -> Tree s
time_tree_to_tree TimeLeaf = Leaf
time_tree_to_tree TimeNode {t_elm=elm, t_fields=fields} =
    Node {elm=elm, children=map (\(_, f) -> time_tree_to_tree f) fields}

query :: (Tree s -> t) -> PartialTree s -> t
query func persistenTree = func (time_tree_to_tree (currentTree persistenTree))
