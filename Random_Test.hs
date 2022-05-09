{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Random_Test where

import System.Random
import System.Random.Shuffle

import DataRecords
import Random_access_list_temporal as RAL_tem
import Random_access_list_persistent as RAL_per
import DAG_construction

import Data.List

import Debug.Trace
import Prettify


rolls :: RandomGen b => Int -> Int -> b -> [Int]
rolls n b = take n . unfoldr (Just . uniformR (0, b))

random_shuffle :: RandomGen b => Int -> b -> [Int]
random_shuffle n = shuffle' [1 .. n] n


build_binary_tree :: TEM_BST t s -> PER_BST t s -> [t] -> ([Tree s], PartialTree s)
build_binary_tree (tem_empty, tem_insert, _) (per_empty, per_insert, _) =
    foldl (\(tem_h : tem_t, per) element ->
            let next_tem = tem_insert element tem_h in
            let next_per = per_insert element per in
            (next_tem : tem_h : tem_t, next_per)
    ) ([tem_empty], per_empty)


build_binary_tree_with_duplicates :: TEM_BST Int s -> PER_BST Int s -> Int -> Int -> ([Tree s], PartialTree s)
build_binary_tree_with_duplicates tem per num seed =
    let pureGen = mkStdGen seed in
    let random_elements = rolls num (2 * num) pureGen in
    build_binary_tree tem per random_elements

build_binary_tree_without_duplicates :: TEM_BST Int s -> PER_BST Int s -> Int -> Int -> ([Tree s], PartialTree s)
build_binary_tree_without_duplicates tem per num seed =
    let pureGen = mkStdGen seed in
    let random_permutation = random_shuffle num pureGen in
    build_binary_tree tem per random_permutation


get_high_out_degree_paths :: Int -> ([Int], [Int])
get_high_out_degree_paths num =
    ( (reverse [num + 2, num + 4 .. 3 * num]) ++ [1] ++ [num + 3, num + 5 .. 3 * num + 1] ++ [0]
    , reverse [2 .. num + 1]
    )


build_binary_persistent_tree_high_out_degree :: PER_BST Int s -> Int -> PartialTree s
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


binary_tree_test_insert :: Eq s => TEM_BST Int s -> PER_BST Int s -> Int -> Bool
binary_tree_test_insert tem per num =
    let (temporal_list, persistent_tree) = build_binary_tree_with_duplicates tem per num 42 in
    let build_persistent_tree = build persistent_tree in

    all (\(test_time, temporal_tree) ->
            temporal_tree == build_persistent_tree test_time
    ) (zip [0 .. num] (reverse temporal_list))



-- TODO: more refractor
-- Refractor creating the trees? They are created almost the same
binary_tree_test_delete :: Show s => Eq s => TEM_BST Int s -> PER_BST Int s -> Int -> Bool
binary_tree_test_delete (tem_empty, tem_insert, tem_delete) (per_empty, per_insert, per_delete) num =
    let insert_seed = 42 in
    let delete_seed = 142 in

    -- Generate elements
    let insert_pureGen = mkStdGen insert_seed in
    let initial_random_elements = rolls num (2 * num) insert_pureGen in

    -- Build random initial tree, using insertion
    let (temporal_base, persistent_base) =
            foldl (\(tem, per) element ->
                    let next_tem = tem_insert element tem in
                    let next_per = per_insert element per in
                    (next_tem, next_per)
            ) (tem_empty, per_empty) initial_random_elements
    in

    -- trace "\n" $
    -- trace (pretty_tree temporal_base) $

    -- Generate elements
    let delete_pureGen = mkStdGen delete_seed in
    let random_elements = rolls num (2 * num) delete_pureGen in

    -- Make deletion on tree
    let (temporal_list, persistent_tree) =
            foldl (\(tem_h : tem_t, per) element ->
                    -- trace (show element) $
                    let next_tem = tem_delete element tem_h in
                    -- trace (pretty_tree next_tem) $
                    -- trace "\n" $
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
binary_tree_high_time_out_degree_node :: Eq s => TEM_BST Int s -> PER_BST Int s -> (PartialTree s -> Int -> Tree s) -> Int -> Bool
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




-- TODO: Comments and refractor building random access lists test


random_access_list_cons :: Int -> Int -> Bool
random_access_list_cons num ins_seed =
    let ins_pureGen = mkStdGen ins_seed in
    let ins_elements = random_shuffle num ins_pureGen in

    let (ins_tem_list, ins_per) =
            foldl (\(tem_h : tem_t, per) element ->
                    let next_tem = RAL_tem.cons element tem_h in
                    let next_per = RAL_per.cons element per in
                    (next_tem : tem_h : tem_t, next_per)
            ) ([RAL_tem.empty], RAL_per.empty) ins_elements
    in

    let build_persistent = build ins_per in

    all (\(test_time, temporal_tree) ->
            temporal_tree == build_persistent test_time
    ) (zip [0 .. num] (reverse ins_tem_list))


random_access_list_update_uniform :: Int -> Int -> Int -> Int -> Bool
random_access_list_update_uniform num ins_seed upd_index_seed upd_elem_seed =
    let ins_pureGen = mkStdGen ins_seed in
    let ins_elements = random_shuffle num ins_pureGen in

    let (ins_tem, ins_per) =
            foldl (\(tem, per) element ->
                    let next_tem = RAL_tem.cons element tem in
                    let next_per = RAL_per.cons element per in
                    (next_tem, next_per)
            ) (RAL_tem.empty, RAL_per.empty) ins_elements
    in
    
    let upd_index_pureGen = mkStdGen upd_index_seed in
    let upd_indexes = rolls num (num - 1) upd_index_pureGen in
    
    let upd_elem_pureGen = mkStdGen upd_elem_seed in
    let upd_elements = rolls num (4 * num) upd_elem_pureGen in

    let (upd_tem_list, upd_per) =
            foldl (\(tem_h : tem_t, per) (index, element) ->
                    let next_tem = RAL_tem.update (index, element) tem_h in
                    let next_per = RAL_per.update (index, element) per in
                    (next_tem : tem_h : tem_t, next_per)
            ) ([ins_tem], ins_per) (zip upd_indexes upd_elements)
    in

    let build_persistent = build upd_per in
    
    let check_time_from = time ins_per - 1 in

    all (\(test_time, temporal_tree) ->
            temporal_tree == build_persistent test_time
    ) (zip [check_time_from .. check_time_from + num] (reverse upd_tem_list))


random_access_list_tail :: Int -> Int -> Bool
random_access_list_tail num ins_seed =
    let ins_pureGen = mkStdGen ins_seed in
    let ins_elements = random_shuffle num ins_pureGen in
    
    let (ins_tem, ins_per) =
            foldl (\(tem, per) element ->
                    let next_tem = RAL_tem.cons element tem in
                    let next_per = RAL_per.cons element per in
                    (next_tem, next_per)
            ) (RAL_tem.empty, RAL_per.empty) ins_elements
    in

    let (tail_tem_list, tail_per) =
            foldl (\(tem_h : tem_t, per) _ ->
                    let next_tem = RAL_tem.tail tem_h in
                    let next_per = RAL_per.tail per in
                    (next_tem : tem_h : tem_t, next_per)
            ) ([ins_tem], ins_per) [0 .. num - 1]
    in

    let build_persistent = build tail_per in
    
    let check_time_from = time ins_per - 1 in

    all (\(test_time, temporal_tree) ->
            temporal_tree == build_persistent test_time
    ) (zip [check_time_from .. check_time_from + num - 1] (reverse tail_tem_list))


random_access_list_update_final_element :: Int -> Int -> Int -> Bool
random_access_list_update_final_element num ins_seed upd_elem_seed =
    let ins_pureGen = mkStdGen ins_seed in
    let ins_elements = random_shuffle num ins_pureGen in

    let (ins_tem, ins_per) =
            foldl (\(tem, per) element ->
                    let next_tem = RAL_tem.cons element tem in
                    let next_per = RAL_per.cons element per in
                    (next_tem, next_per)
            ) (RAL_tem.empty, RAL_per.empty) ins_elements
    in
    
    let upd_elem_pureGen = mkStdGen upd_elem_seed in
    let upd_elements = rolls num (4 * num) upd_elem_pureGen in

    let (upd_tem_list, upd_per) =
            foldl (\(tem_h : tem_t, per) element ->
                    let next_tem = RAL_tem.update (num - 1, element) tem_h in
                    let next_per = RAL_per.update (num - 1, element) per in
                    (next_tem : tem_h : tem_t, next_per)
            ) ([ins_tem], ins_per) upd_elements
    in

    let build_persistent = build upd_per in
    
    let check_time_from = time ins_per - 1 in

    all (\(test_time, temporal_tree) ->
            temporal_tree == build_persistent test_time
    ) (zip [check_time_from .. check_time_from + num] (reverse upd_tem_list))
