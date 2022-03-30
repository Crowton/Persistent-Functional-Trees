module Main where

import Data.Function

import qualified Data.Map.Strict as MB

import DataRecords as D
import PartialTree_Create as Crea
import DAG_construction as C
-- import Query as Q
-- import Tree_Constructor as TC

import Binary_Tree_temporal as TEM
import Binary_Tree_persistent_mock as PER
import Prettify

import Debug.Trace

-- stupidContains :: MB.Map Int (FrozenNode Int) -> Int -> Int -> Bool
-- stupidContains rootMap time key =
--     let func info fieldQuery = case info of
--             Nothing -> False
--             Just value -> 
--                 -- trace ("running..." ++ show value) $
--                 value == key || fieldQuery 0 || fieldQuery 1
--     in
    
--     query func rootMap time

stupidContains2 :: (Int -> Tree Int) -> Int -> Int -> Bool
stupidContains2 treeGen time key =
    let contains tree = case tree of
            Leaf -> False
            Node {elm=elm, children=children} -> 
                elm == key || (any contains children)
    in
    contains (treeGen time)

main = do
    let treeGen = C.build (PartialTree
                { edgeFreezer = [ D.TimeEdge {id_from = 0, field = 0, id_to = 1, time_from = 2, time_to = 4}
                    , D.TimeEdge {id_from = 1, field = 1, id_to = 2, time_from = 1, time_to = 2}
                    , D.TimeEdge {id_from = 0, field = 1, id_to = 2, time_from = 2, time_to = 4}
                    , D.TimeEdge {id_from = 2, field = 1, id_to = 3, time_from = 3, time_to = 4}
                    , D.TimeEdge {id_from = 1, field = 1, id_to = 4, time_from = 3, time_to = 4}
                    ]
                , idStaticList = [(0, 0 :: Int), (1, 1 :: Int), (2, 2 :: Int), (3, 3 :: Int), (4, 4 :: Int)]
                , rootList = [(1, 1), (2, 0)]
                , idCount = 5
                , fieldCount = 2
                , time = 4
                , currentTree = TimeLeaf})
    -- print (MB.lookupLE 4 graph)
    -- print (stupidContains graph 3 3)
    -- print (stupidContains2 treeGen 3 1)
    -- let tree = Node {elm=2, children=[Node {elm=1, children=[Leaf, Leaf]}, Leaf]}
    let tree = Leaf
                & TEM.insert 3
                & TEM.insert 1
                & TEM.insert 2
                & TEM.insert 4
                & TEM.delete 3
    putStrLn (pretty_tree tree)
    print (TEM.contains 1 tree)
