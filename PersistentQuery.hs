{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module PersistentQuery where

import DataRecords


-- Convert instance of TimeTree to Tree, by stripping time and id information
timeTreeToTree :: TimeTree s -> Tree s
timeTreeToTree TimeLeaf = Leaf
timeTreeToTree TimeNode {tElm=elm, tFields=fields} =
    Node {elm=elm, children=map (\(_, f) -> timeTreeToTree f) fields}

-- Query by querying the striped tree
query :: (u -> Tree s -> t) -> u -> PartialTree s -> t
query func val persistenTree = func val (timeTreeToTree (currentTree persistenTree))
