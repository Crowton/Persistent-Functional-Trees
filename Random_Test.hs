{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Random_Test where

import System.Random
import System.Random.Shuffle

import DataRecords
import Binary_Tree_temporal as TEM
import Binary_Tree_persistent_mock as PER_M
import Binary_Tree_persistent as PER
import DAG_construction

import Data.List
import Debug.Trace

import Prettify


rolls :: RandomGen b => Int -> b -> [Int]
rolls n = take n . unfoldr (Just . uniformR (0, 2 * n))


random_shuffle :: RandomGen b => Int -> b -> [Int]
random_shuffle n = shuffle' [1 .. n] n


-- TODO: Refractor this to be readable
-- and aboid this much code duplication
-- Maybe move mock code away, and overwrite? let git handle it?
build_mock_binary_tree :: [Int] -> ([Tree Int], PartialTree Int)
build_mock_binary_tree =
    foldl (\(tem_h : tem_t, per) element ->
            let next_tem = TEM.insert element tem_h in
            let next_per = PER_M.insert element per in
            (next_tem : tem_h : tem_t, next_per)
    ) ([Leaf], PER_M.construct_empty_tree)


build_mock_binary_tree_with_duplicates :: Int -> Int -> ([Tree Int], PartialTree Int)
build_mock_binary_tree_with_duplicates num seed =
    let pureGen = mkStdGen seed in
    let random_elements = rolls num pureGen in
    build_mock_binary_tree random_elements

build_mock_binary_tree_without_duplicates :: Int -> Int -> ([Tree Int], PartialTree Int)
build_mock_binary_tree_without_duplicates num seed =
    let pureGen = mkStdGen seed in
    let random_permutation = random_shuffle num pureGen in
    build_mock_binary_tree random_permutation



build_binary_tree :: [Int] -> ([Tree Int], PartialTree Int)
build_binary_tree =
    foldl (\(tem_h : tem_t, per) element ->
            let next_tem = TEM.insert element tem_h in
            let next_per = PER.insert element per in
            (next_tem : tem_h : tem_t, next_per)
    ) ([Leaf], PER.empty)


build_binary_tree_with_duplicates :: Int -> Int -> ([Tree Int], PartialTree Int)
build_binary_tree_with_duplicates num seed =
    let pureGen = mkStdGen seed in
    let random_elements = rolls num pureGen in
    build_binary_tree random_elements




build_mock_binary_persistent_tree_high_out_degree :: Int -> PartialTree Int
build_mock_binary_persistent_tree_high_out_degree num =
    let first_path = (reverse [num + 2, num + 4 .. 3 * num]) ++ [1] ++ [num + 3, num + 5 .. 3 * num + 1] ++ [0] in
    let second_path = reverse [2 .. num + 1] in

    -- Build initial tree, using insertion
    let persistent_base =
            foldl (\per element ->
                    PER_M.insert element per
            ) PER_M.construct_empty_tree (first_path ++ second_path)
    in

    -- Make deletion of the second path
    let persistent_tree =
            foldl (\per element ->
                    PER_M.delete element per
            ) persistent_base second_path
    in

    persistent_tree


binary_tree_test_mock_insert :: Int -> Bool
binary_tree_test_mock_insert num =
    let (temporal_list, persistent_tree) = build_mock_binary_tree_with_duplicates num 42 in
    let build_persistent_tree = build persistent_tree in

    all (\(test_time, temporal_tree) ->
            temporal_tree == build_persistent_tree test_time
    ) (zip [0 .. num] (reverse temporal_list))


binary_tree_test_insert :: Int -> Bool
binary_tree_test_insert num =
    let (temporal_list, persistent_tree) = build_binary_tree_with_duplicates num 42 in
    let build_persistent_tree = build persistent_tree in

    all (\(test_time, temporal_tree) ->
            temporal_tree == build_persistent_tree test_time
    ) (zip [0 .. num] (reverse temporal_list))



binary_tree_test_mock_delete :: Int -> Bool
binary_tree_test_mock_delete num =
    let insert_seed = 42 in
    let delete_seed = 142 in

    -- Generate elements
    let insert_pureGen = mkStdGen insert_seed in
    let initial_random_elements = rolls num insert_pureGen in

    -- Build random initial tree, using insertion
    let (temporal_base, persistent_base) =
            foldl (\(tem, per) element ->
                    let next_tem = TEM.insert element tem in
                    let next_per = PER_M.insert element per in
                    (next_tem, next_per)
            ) (Leaf, PER_M.construct_empty_tree) initial_random_elements
    in

    -- Generate elements
    let delete_pureGen = mkStdGen delete_seed in
    let random_elements = rolls num delete_pureGen in

    -- Make deletion on tree
    let (temporal_list, persistent_tree) =
            foldl (\(tem_h : tem_t, per) element ->
                    let next_tem = TEM.delete element tem_h in
                    let next_per = PER_M.delete element per in
                    (next_tem : tem_h : tem_t, next_per)
            ) ([temporal_base], persistent_base) random_elements
    in

    -- Build tree
    let build_persistent_tree = build persistent_tree in

    -- Fetch time before the first deletion
    let check_time_from = time persistent_base - 1 in

    -- Check equality
    all (\(test_time, temporal_tree) ->
            (if temporal_tree /= build_persistent_tree test_time
                    then trace ("Fails at time: " ++ show test_time)
                    else id) $
            temporal_tree == build_persistent_tree test_time
    ) (zip [check_time_from .. check_time_from + num] (reverse temporal_list))


binary_tree_test_delete :: Int -> Bool
binary_tree_test_delete num =
    let insert_seed = 42 in
    let delete_seed = 142 in

    trace "\n" $

    -- Generate elements
    let insert_pureGen = mkStdGen insert_seed in
    let initial_random_elements = rolls num insert_pureGen in
    
    trace ("Generation items:\n" ++ (show initial_random_elements) ++ "\n") $

    -- Build random initial tree, using insertion
    let (temporal_base, persistent_base) =
            foldl (\(tem, per) element ->
                    let next_tem = TEM.insert element tem in
                    let next_per = PER.insert element per in
                    (next_tem, next_per)
            ) (Leaf, PER.empty) initial_random_elements
    in
    
    trace ("Initial tem:\n" ++ (pretty_tree temporal_base) ++ "\n") $
    trace ("Initial per:\n" ++ (pretty_tree (build persistent_base (time persistent_base - 1))) ++ "\n") $

    -- Generate elements
    let delete_pureGen = mkStdGen delete_seed in
    let random_elements = rolls num delete_pureGen in

    trace ("Deletion items:\n" ++ (show random_elements) ++ "\n") $

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

    trace ("Next Couple per:\n" ++ (pretty_tree (build_persistent_tree (time persistent_base - 1))) ++ "\n") $
    trace ("Next Couple per:\n" ++ (pretty_tree (build_persistent_tree (time persistent_base))) ++ "\n") $
    trace ("Next Couple per:\n" ++ (pretty_tree (build_persistent_tree (time persistent_base + 1))) ++ "\n") $
    trace ("Next Couple per:\n" ++ (pretty_tree (build_persistent_tree (time persistent_base + 2))) ++ "\n") $
    trace ("Next Couple per:\n" ++ (pretty_tree (build_persistent_tree (time persistent_base + 3))) ++ "\n") $
    trace ("Next Couple per:\n" ++ (pretty_tree (build_persistent_tree (time persistent_base + 4))) ++ "\n") $
    trace ("Next Couple per:\n" ++ (pretty_tree (build_persistent_tree (time persistent_base + 5))) ++ "\n") $

    -- Fetch time before the first deletion
    let check_time_from = time persistent_base - 1 in

    -- Check equality
    all (\(test_time, temporal_tree) ->
            let time_persistent_tree = build_persistent_tree test_time in
            (if temporal_tree /= time_persistent_tree
                    then trace ("Fails at time: " ++ show (test_time - check_time_from)
                                    ++ "\nTemporal:\n" ++ (pretty_tree temporal_tree)
                                    ++ "\n\nPersistent:\n" ++ (pretty_tree time_persistent_tree))
                    else id) $
            temporal_tree == time_persistent_tree
    ) (zip [check_time_from .. check_time_from + num] (reverse temporal_list))


-- Tree contains long left path, one path to the right, and another left path
-- By deleting the element to the right repeatly, the parent gets high out degree,
-- which the dag construction then needs to make smaller
-- To make it worse, each node have a second other child, which is never touched, to force more splits
binary_tree_mock_high_time_out_degree_node :: (PartialTree Int -> Int -> Tree Int) -> Int -> Bool
binary_tree_mock_high_time_out_degree_node builder num =
    let first_path = (reverse [num + 2, num + 4 .. 3 * num]) ++ [1] ++ [num + 3, num + 5 .. 3 * num + 1] ++ [0] in
    let second_path = reverse [2 .. num + 1] in

    -- Build initial tree, using insertion
    let (temporal_base, persistent_base) =
            foldl (\(tem, per) element ->
                    let next_tem = TEM.insert element tem in
                    let next_per = PER_M.insert element per in
                    (next_tem, next_per)
            ) (Leaf, PER_M.construct_empty_tree) (first_path ++ second_path)
    in

    -- Make deletion of the second path
    let (temporal_list, persistent_tree) =
            foldl (\(tem_h : tem_t, per) element ->
                    let next_tem = TEM.delete element tem_h in
                    let next_per = PER_M.delete element per in
                    (next_tem : tem_h : tem_t, next_per)
            ) ([temporal_base], persistent_base) second_path
    in

    -- Build tree
    -- let build_persistent_tree = build persistent_tree in
    -- let build_persistent_tree = build_node_split persistent_tree in
    let build_persistent_tree = builder persistent_tree in

    -- Fetch time before the first deletion
    let check_time_from = time persistent_base - 1 in

    -- Check equality
    all (\(test_time, temporal_tree) ->
            temporal_tree == build_persistent_tree test_time
    ) (zip [check_time_from .. check_time_from + num] (reverse temporal_list))


binary_tree_high_time_out_degree_node :: (PartialTree Int -> Int -> Tree Int) -> Int -> Bool
binary_tree_high_time_out_degree_node builder num =
    let first_path = (reverse [num + 2, num + 4 .. 3 * num]) ++ [1] ++ [num + 3, num + 5 .. 3 * num + 1] ++ [0] in
    let second_path = reverse [2 .. num + 1] in

    -- Build initial tree, using insertion
    let (temporal_base, persistent_base) =
            foldl (\(tem, per) element ->
                    let next_tem = TEM.insert element tem in
                    let next_per = PER.insert element per in
                    (next_tem, next_per)
            ) (Leaf, PER.empty) (first_path ++ second_path)
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
    -- let build_persistent_tree = build persistent_tree in
    -- let build_persistent_tree = build_node_split persistent_tree in
    let build_persistent_tree = builder persistent_tree in

    -- Fetch time before the first deletion
    let check_time_from = time persistent_base - 1 in

    -- Check equality
    all (\(test_time, temporal_tree) ->
            temporal_tree == build_persistent_tree test_time
    ) (zip [check_time_from .. check_time_from + num] (reverse temporal_list))
