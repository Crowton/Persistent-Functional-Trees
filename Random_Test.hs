{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wall #-}

module Random_Test where

import System.Random

import DataRecords
import Binary_Tree_temporal as TEM
import Binary_Tree_persistent as PER
import DAG_construction

import Data.List


rolls :: RandomGen b => Int -> b -> [Int]
rolls n = take n . unfoldr (Just . uniformR (0, 2 * n))


binary_tree_test_insert :: Int -> Bool
binary_tree_test_insert num =
    let seed = 42 in
    let pureGen = mkStdGen seed in
    let random_elements = rolls num pureGen in

    let (temporal_list, persistent_tree) = 
            foldl (\(tem_h : tem_t, per) element ->
                    let next_tem = TEM.insert element tem_h in
                    let next_per = PER.insert element per in
                    (next_tem : tem_h : tem_t, next_per)
            ) ([Leaf], PER.construct_empty_tree) random_elements
    in
    
    let build_persistent_tree = build persistent_tree in

    all (\(test_time, temporal_tree) ->
            temporal_tree == (build_persistent_tree test_time)
    ) (zip [0 .. num] (reverse temporal_list))


binary_tree_test_delete :: Int -> Bool
binary_tree_test_delete num =
    let insert_seed = 42 in
    let delete_seed = 142 in
    
    -- Generate elements
    let insert_pureGen = mkStdGen insert_seed in
    let initial_random_elements = rolls num insert_pureGen in
    
    -- Build random initial tree, using insertion
    let (temporal_base, persistent_base) =
            foldl (\(tem, per) element ->
                    let next_tem = TEM.insert element tem in
                    let next_per = PER.insert element per in
                    (next_tem, next_per)
            ) (Leaf, PER.construct_empty_tree) initial_random_elements
    in

    -- Generate elements
    let delete_pureGen = mkStdGen delete_seed in
    let random_elements = rolls num delete_pureGen in

    -- Make deletion on tree
    let (temporal_list, persistent_tree) = 
            foldl (\(tem_h : tem_t, per) element ->
                    let next_tem = TEM.delete element tem_h in
                    let next_per = PER.delete element per in
                    (next_tem : tem_h : tem_t, next_per)
            ) ([temporal_base], persistent_base) random_elements
    in
    
    -- Build tree
    let build_persistent_tree = build persistent_tree in

    -- Fetch time before the first deletion
    let check_time_from = (time persistent_base) - 1 in

    -- Check equality
    all (\(test_time, temporal_tree) ->
            temporal_tree == (build_persistent_tree test_time)
    ) (zip [check_time_from .. check_time_from + num] (reverse temporal_list))


-- Tree contains long left path, one path to the right, and another left path
-- By deleting the element to the right repeatly, the parent gets high out degree,
-- which the dag construction then needs to make smaller
binary_tree_high_time_out_degree_node :: Int -> Bool
binary_tree_high_time_out_degree_node num =
    let first_path = (reverse [num + 2 .. 2 * num]) ++ [1] in
    let second_path = reverse [2 .. num + 1] in

    -- Build initial tree, using insertion
    let (temporal_base, persistent_base) =
            foldl (\(tem, per) element ->
                    let next_tem = TEM.insert element tem in
                    let next_per = PER.insert element per in
                    (next_tem, next_per)
            ) (Leaf, PER.construct_empty_tree) (first_path ++ second_path)
    in
    
    -- Make deletion of the second path
    let (temporal_list, persistent_tree) = 
            foldl (\(tem_h : tem_t, per) element ->
                    let next_tem = TEM.delete element tem_h in
                    let next_per = PER.delete element per in
                    (next_tem : tem_h : tem_t, next_per)
            ) ([temporal_base], persistent_base) second_path
    in
    
    -- Build tree
    let build_persistent_tree = build persistent_tree in

    -- Fetch time before the first deletion
    let check_time_from = (time persistent_base) - 1 in

    -- Check equality
    all (\(test_time, temporal_tree) ->
            temporal_tree == (build_persistent_tree test_time)
    ) (zip [check_time_from .. check_time_from + num] (reverse temporal_list))
