{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wall #-}

module Random_Test where

import System.Random

import Debug.Trace

import DataRecords
import Binary_Tree_temporal as TEM
import Binary_Tree_persistent_mock as PER
import DAG_construction

import Data.List


binary_tree_test_insert :: Int -> Bool
binary_tree_test_insert num =
    let seed = 42 in
    let pureGen = mkStdGen seed in
    let rolls n = take n . unfoldr (Just . uniformR (0, 2 * n)) in
    let random_elements = rolls num pureGen in

    let (temporal_list, persistent_tree) = 
            foldl (\(tem_h : tem_t, per) element ->
                    -- trace ("Random insert! " ++ (show element)) $
                    let next_tem = TEM.insert element tem_h in
                    let next_per = PER.insert element per in
                    (next_tem : tem_h : tem_t, next_per)
            ) ([Leaf], PER.construct_empty_tree) random_elements
    in
    
    let build_persistent_tree = build persistent_tree in

    all (\(test_time, temporal_tree) ->
            -- trace ("Time: " ++ (show test_time)) $
            -- trace ("Temporal:   " ++ (show temporal_tree)) $
            -- trace ("Persistent: " ++ (show (build_persistent_tree test_time))) $
            temporal_tree == (build_persistent_tree test_time)
    ) (zip [0 .. num] (reverse temporal_list))


binary_tree_test_also_delete :: Int -> Bool
binary_tree_test_also_delete num =
    let seed = 42 in
    let pureGen = mkStdGen seed in
    let rolls n = take n . unfoldr (Just . uniformR (0, 2 * n)) in
    let random_elements = rolls num pureGen in
    let opr_typ n = take n . unfoldr (Just . uniformR (0 :: Int, 10 :: Int)) in
    let random_opr = opr_typ num pureGen in

    let (temporal_list, persistent_tree) = 
            foldl (\(tem_h : tem_t, per) (element, opr) ->
                    if opr < (4 :: Int)
                        then let next_tem = TEM.insert element tem_h in
                             let next_per = PER.insert element per in
                             (next_tem : tem_h : tem_t, next_per)
                        else let next_tem = TEM.delete element tem_h in
                             let next_per = PER.delete element per in
                             (next_tem : tem_h : tem_t, next_per)
            ) ([Leaf], PER.construct_empty_tree) (zip random_elements random_opr)
    in
    
    let build_persistent_tree = build persistent_tree in

    all (\(test_time, temporal_tree) ->
            -- trace ("Time: " ++ (show test_time)) $
            -- trace ("Temporal:   " ++ (show temporal_tree)) $
            -- trace ("Persistent: " ++ (show (build_persistent_tree test_time))) $
            temporal_tree == (build_persistent_tree test_time)
    ) (zip [0 .. num] (reverse temporal_list))
