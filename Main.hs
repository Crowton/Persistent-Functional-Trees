module Main where

import qualified Data.Map.Strict as MB

import DataRecords as D
import DAG_construction as C
import Query as Q
import Tree_Constructor as TC

import Debug.Trace

stupidContains :: MB.Map Int (FrozenNode Int) -> Int -> Int -> Bool
stupidContains rootMap time key =
    let func info fieldQuery = case info of
            Nothing -> False
            Just value -> 
                -- trace ("running..." ++ show value) $
                value == key || fieldQuery 0 || fieldQuery 1
    in
    
    query func rootMap time

stupidContains2 :: (Int -> Tree Int) -> Int -> Int -> Bool
stupidContains2 treeGen time key =
    let contains tree = case tree of
            Leaf -> False
            Node {elm=elm, children=children} -> 
                elm == key || (any contains children)
    in
    contains (treeGen time)

main = do
    let graph = C.build
                [ D.TimeEdge {id_from = 0, field = 0, id_to = 1, time_from = 2, time_to = 4}
                , D.TimeEdge {id_from = 1, field = 1, id_to = 2, time_from = 1, time_to = 2}
                , D.TimeEdge {id_from = 0, field = 1, id_to = 2, time_from = 2, time_to = 4}
                , D.TimeEdge {id_from = 2, field = 1, id_to = 3, time_from = 3, time_to = 4}
                , D.TimeEdge {id_from = 1, field = 1, id_to = 4, time_from = 3, time_to = 4}
                ]
                [(0, 0 :: Int), (1, 1 :: Int), (2, 2 :: Int), (3, 3 :: Int), (4, 4 :: Int)]
                [(1, 1), (2, 0)]
                5 2
    -- print (MB.lookupLE 4 graph)
    print (stupidContains graph 3 3)
    print (stupidContains2 (construct graph) 3 1)
