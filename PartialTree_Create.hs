module PartialTree_Create where

import DataRecords as D


construct_empty_tree :: Int -> PartialTree s
construct_empty_tree fieldCount =
    PartialTree {
        edgeFreezer = [],
        idStaticList = [],
        rootList = [],
        idCount = 0,
        fieldCount = fieldCount,
        time = 0,
        currentTree = Leaf
    }



