{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wall #-}

module Random_Test where

import System.Random

import Debug.Trace

import DataRecords
import Binary_Tree_temporal as TEM
import Binary_Tree_persistent_mock as PER
import DAG_construction


binary_tree_test :: Int -> Bool
binary_tree_test num =
    let (temporal_list, persistent_tree) = 
            foldl (\(tem_h : tem_t, per) i ->
                    let new_element = i in --uniformR (1 :: Int, 10 :: Int) in
                    -- trace ("Random insert! " ++ (show new_element)) $
                    let next_tem = TEM.insert new_element tem_h in
                    let next_per = PER.insert new_element per in
                    (next_tem : tem_h : tem_t, next_per)
            ) ([Leaf], PER.construct_empty_tree) [1 .. num]
    in
    
    let build_persistent_tree = build persistent_tree in

    all (\(test_time, temporal_tree) ->
            -- trace ("Time: " ++ (show test_time)) $
            -- trace ("Temporal:   " ++ (show temporal_tree)) $
            -- trace ("Persistent: " ++ (show (build_persistent_tree test_time))) $
            temporal_tree == (build_persistent_tree test_time)
    ) (zip [0 .. num] (reverse temporal_list))
