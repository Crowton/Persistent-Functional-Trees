{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Random_Test where

import System.Random
import System.Random.Shuffle

import DataRecords
import DAG_construction

import Data.List

import Prettify


rolls :: RandomGen b => Int -> b -> [Int]
rolls n = take n . unfoldr (Just . uniformR (0, 2 * n))

random_shuffle :: RandomGen b => Int -> b -> [Int]
random_shuffle n = shuffle' [1 .. n] n


build_binary_tree :: TEM_BST s -> PER_BST s -> [s] -> ([Tree s], PartialTree s)
build_binary_tree (tem_empty, tem_insert, _) (per_empty, per_insert, _) =
    foldl (\(tem_h : tem_t, per) element ->
            let next_tem = tem_insert element tem_h in
            let next_per = per_insert element per in
            (next_tem : tem_h : tem_t, next_per)
    ) ([tem_empty], per_empty)


build_binary_tree_with_duplicates :: TEM_BST Int -> PER_BST Int -> Int -> Int -> ([Tree Int], PartialTree Int)
build_binary_tree_with_duplicates tem per num seed =
    let pureGen = mkStdGen seed in
    let random_elements = rolls num pureGen in
    build_binary_tree tem per random_elements

build_binary_tree_without_duplicates :: TEM_BST Int -> PER_BST Int -> Int -> Int -> ([Tree Int], PartialTree Int)
build_binary_tree_without_duplicates tem per num seed =
    let pureGen = mkStdGen seed in
    let random_permutation = random_shuffle num pureGen in
    build_binary_tree tem per random_permutation


get_high_out_degree_paths :: Int -> ([Int], [Int])
get_high_out_degree_paths num =
    ( (reverse [num + 2, num + 4 .. 3 * num]) ++ [1] ++ [num + 3, num + 5 .. 3 * num + 1] ++ [0]
    , reverse [2 .. num + 1]
    )


build_binary_persistent_tree_high_out_degree :: PER_BST Int -> Int -> PartialTree Int
build_binary_persistent_tree_high_out_degree (per_empty, per_insert, per_delete) num =
    let (first_path, second_path) = get_high_out_degree_paths num in

    -- Build initial tree, using insertion
    let persistent_base =
            foldl (\per element ->
                    per_insert element per
            ) per_empty (first_path ++ second_path)
    in

    -- Make deletion of the second path
    let persistent_tree =
            foldl (\per element ->
                    per_delete element per
            ) persistent_base second_path
    in

    persistent_tree


binary_tree_test_insert :: TEM_BST Int -> PER_BST Int -> Int -> Bool
binary_tree_test_insert tem per num =
    let (temporal_list, persistent_tree) = build_binary_tree_with_duplicates tem per num 42 in
    let build_persistent_tree = build persistent_tree in

    all (\(test_time, temporal_tree) ->
            temporal_tree == build_persistent_tree test_time
    ) (zip [0 .. num] (reverse temporal_list))



-- TODO: mere refractorx
-- Refractor at lave træerne? Meget af det er ens
binary_tree_test_delete :: TEM_BST Int -> PER_BST Int -> Int -> Bool
binary_tree_test_delete (tem_empty, tem_insert, tem_delete) (per_empty, per_insert, per_delete) num =
    let insert_seed = 42 in
    let delete_seed = 142 in

    -- Generate elements
    let insert_pureGen = mkStdGen insert_seed in
    let initial_random_elements = rolls num insert_pureGen in

    -- Build random initial tree, using insertion
    let (temporal_base, persistent_base) =
            foldl (\(tem, per) element ->
                    let next_tem = tem_insert element tem in
                    let next_per = per_insert element per in
                    (next_tem, next_per)
            ) (tem_empty, per_empty) initial_random_elements
    in

    -- Generate elements
    let delete_pureGen = mkStdGen delete_seed in
    let random_elements = rolls num delete_pureGen in

    -- Make deletion on tree
    let (temporal_list, persistent_tree) =
            foldl (\(tem_h : tem_t, per) element ->
                    let next_tem = tem_delete element tem_h in
                    let next_per = per_delete element per in
                    (next_tem : tem_h : tem_t, next_per)
            ) ([temporal_base], persistent_base) random_elements
    in

    -- Build tree
    let build_persistent_tree = build persistent_tree in

    -- Fetch time before the first deletion
    let check_time_from = time persistent_base - 1 in

    -- Check equality
    all (\(test_time, temporal_tree) ->
            temporal_tree == build_persistent_tree test_time
    ) (zip [check_time_from .. check_time_from + num] (reverse temporal_list))


-- Tree contains long left path, one path to the right, and another left path
-- By deleting the element to the right repeatly, the parent gets high out degree,
-- which the dag construction then needs to make smaller
-- To make it worse, each node have a second other child, which is never touched, to force more splits
binary_tree_high_time_out_degree_node :: TEM_BST Int -> PER_BST Int -> (PartialTree Int -> Int -> Tree Int) -> Int -> Bool
binary_tree_high_time_out_degree_node (tem_empty, tem_insert, tem_delete) (per_empty, per_insert, per_delete) builder num =
    let (first_path, second_path) = get_high_out_degree_paths num in

    -- Build initial tree, using insertion
    let (temporal_base, persistent_base) =
            foldl (\(tem, per) element ->
                    let next_tem = tem_insert element tem in
                    let next_per = per_insert element per in
                    (next_tem, next_per)
            ) (tem_empty, per_empty) (first_path ++ second_path)
    in

    -- Make deletion of the second path
    let (temporal_list, persistent_tree) =
            foldl (\(tem_h : tem_t, per) element ->
                    let next_tem = tem_delete element tem_h in
                    let next_per = per_delete element per in
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
