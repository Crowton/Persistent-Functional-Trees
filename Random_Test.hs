{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Random_Test where

import System.Random
import System.Random.Shuffle

import DataRecords
import Random_access_list_ephemeral as RAL_eph
import Random_access_list_persistent as RAL_per
import DAG_construction

import Data.List


rolls :: RandomGen b => Int -> Int -> b -> [Int]
rolls n b = take n . unfoldr (Just . uniformR (0, b))

random_shuffle :: RandomGen b => Int -> b -> [Int]
random_shuffle n = shuffle' [1 .. n] n


apply_binary_tree_updates :: (t -> Tree s -> Tree s) -> (t -> PartialTree s -> PartialTree s) -> ([Tree s], PartialTree s) -> [t] -> ([Tree s], PartialTree s)
apply_binary_tree_updates eph_update per_update =
    foldl (\(eph_h : eph_t, per) element ->
            let next_eph = eph_update element eph_h in
            let next_per = per_update element per in
            (next_eph : eph_h : eph_t, next_per)
    )

build_binary_tree :: EPH_BST t s -> PER_BST t s -> [t] -> ([Tree s], PartialTree s)
build_binary_tree (eph_empty, eph_insert, _) (per_empty, per_insert, _) =
    apply_binary_tree_updates eph_insert per_insert ([eph_empty], per_empty)

debuild_binary_tree :: EPH_BST t s -> PER_BST t s -> ([Tree s], PartialTree s) -> [t] -> ([Tree s], PartialTree s)
debuild_binary_tree (_, _, eph_delete) (_, _, per_delete) =
    apply_binary_tree_updates eph_delete per_delete


build_binary_tree_with_duplicates :: EPH_BST Int s -> PER_BST Int s -> Int -> Int -> ([Tree s], PartialTree s)
build_binary_tree_with_duplicates eph per num seed =
    let pureGen = mkStdGen seed in
    let random_elements = rolls num (2 * num) pureGen in
    build_binary_tree eph per random_elements

build_binary_tree_without_duplicates :: EPH_BST Int s -> PER_BST Int s -> Int -> Int -> ([Tree s], PartialTree s)
build_binary_tree_without_duplicates eph per num seed =
    let pureGen = mkStdGen seed in
    let random_permutation = random_shuffle num pureGen in
    build_binary_tree eph per random_permutation

destroy_binary_tree_without_duplicates :: EPH_BST Int s -> PER_BST Int s -> Int -> Int -> ([Tree s], PartialTree s) -> ([Tree s], PartialTree s)
destroy_binary_tree_without_duplicates eph per num seed initial_trees =
    let pureGen = mkStdGen seed in
    let random_permutation = random_shuffle num pureGen in
    debuild_binary_tree eph per initial_trees random_permutation


build_and_destroy_binary_tree_without_duplicates :: EPH_BST Int s -> PER_BST Int s -> Int -> Int -> ([Tree s], PartialTree s)
build_and_destroy_binary_tree_without_duplicates eph per num seed =
    destroy_binary_tree_without_duplicates eph per num (-seed) (build_binary_tree_without_duplicates eph per num seed)


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


binary_tree_test_insert :: (Show s, Eq s) => EPH_BST Int s -> PER_BST Int s -> Int -> Bool -- TODO: remove show
binary_tree_test_insert eph per num =
    let (ephemeral_list, persistent_tree) = build_binary_tree_with_duplicates eph per num 42 in
    let build_persistent_tree = build persistent_tree in

    all (\(test_time, ephemeral_tree) ->
            ephemeral_tree == build_persistent_tree test_time
    ) (zip [0 .. num] (reverse ephemeral_list))



-- TODO: more refractor
-- Refractor creating the trees? They are created almost the same
binary_tree_test_delete :: Show s => Eq s => EPH_BST Int s -> PER_BST Int s -> Int -> Bool
binary_tree_test_delete (eph_empty, eph_insert, eph_delete) (per_empty, per_insert, per_delete) num =
    let insert_seed = 42 in
    let delete_seed = 142 in

    -- Generate elements
    let insert_pureGen = mkStdGen insert_seed in
    let initial_random_elements = rolls num (2 * num) insert_pureGen in

    -- Build random initial tree, using insertion
    let (ephemeral_base, persistent_base) =
            foldl (\(eph, per) element ->
                    let next_eph = eph_insert element eph in
                    let next_per = per_insert element per in
                    (next_eph, next_per)
            ) (eph_empty, per_empty) initial_random_elements
    in

    -- Generate elements
    let delete_pureGen = mkStdGen delete_seed in
    let random_elements = rolls num (2 * num) delete_pureGen in

    -- Make deletion on tree
    let (ephemeral_list, persistent_tree) =
            foldl (\(eph_h : eph_t, per) element ->
                    let next_eph = eph_delete element eph_h in
                    let next_per = per_delete element per in
                    (next_eph : eph_h : eph_t, next_per)
            ) ([ephemeral_base], persistent_base) random_elements
    in

    -- Build tree
    let build_persistent_tree = build persistent_tree in

    -- Fetch time before the first deletion
    let check_time_from = time persistent_base - 1 in

    -- Check equality
    all (\(test_time, ephemeral_tree) ->
            ephemeral_tree == build_persistent_tree test_time
    ) (zip [check_time_from .. check_time_from + num] (reverse ephemeral_list))


-- Tree contains long left path, one path to the right, and another left path
-- By deleting the element to the right repeatly, the parent gets high out degree,
-- which the dag construction then needs to make smaller
-- To make it worse, each node have a second other child, which is never touched, to force more splits
binary_tree_high_time_out_degree_node :: Eq s => EPH_BST Int s -> PER_BST Int s -> (PartialTree s -> Int -> Tree s) -> Int -> Bool
binary_tree_high_time_out_degree_node (eph_empty, eph_insert, eph_delete) (per_empty, per_insert, per_delete) builder num =
    let (first_path, second_path) = get_high_out_degree_paths num in

    -- Build initial tree, using insertion
    let (ephemeral_base, persistent_base) =
            foldl (\(eph, per) element ->
                    let next_eph = eph_insert element eph in
                    let next_per = per_insert element per in
                    (next_eph, next_per)
            ) (eph_empty, per_empty) (first_path ++ second_path)
    in

    -- Make deletion of the second path
    let (ephemeral_list, persistent_tree) =
            foldl (\(eph_h : eph_t, per) element ->
                    let next_eph = eph_delete element eph_h in
                    let next_per = per_delete element per in
                    (next_eph : eph_h : eph_t, next_per)
            ) ([ephemeral_base], persistent_base) second_path
    in

    -- Build tree
    -- let build_persistent_tree = build persistent_tree in
    -- let build_persistent_tree = build_node_split persistent_tree in
    let build_persistent_tree = builder persistent_tree in

    -- Fetch time before the first deletion
    let check_time_from = time persistent_base - 1 in

    -- Check equality
    all (\(test_time, ephemeral_tree) ->
            ephemeral_tree == build_persistent_tree test_time
    ) (zip [check_time_from .. check_time_from + num] (reverse ephemeral_list))




-- TODO: Comments and refractor building random access lists test


random_access_list_cons :: Int -> Int -> Bool
random_access_list_cons num ins_seed =
    let ins_pureGen = mkStdGen ins_seed in
    let ins_elements = random_shuffle num ins_pureGen in

    let (ins_eph_list, ins_per) =
            foldl (\(eph_h : eph_t, per) element ->
                    let next_eph = RAL_eph.cons element eph_h in
                    let next_per = RAL_per.cons element per in
                    (next_eph : eph_h : eph_t, next_per)
            ) ([RAL_eph.empty], RAL_per.empty) ins_elements
    in

    let build_persistent = build ins_per in

    all (\(test_time, ephemeral_tree) ->
            ephemeral_tree == build_persistent test_time
    ) (zip [0 .. num] (reverse ins_eph_list))


random_access_list_update_uniform :: Int -> Int -> Int -> Int -> Bool
random_access_list_update_uniform num ins_seed upd_index_seed upd_elem_seed =
    let ins_pureGen = mkStdGen ins_seed in
    let ins_elements = random_shuffle num ins_pureGen in

    let (ins_eph, ins_per) =
            foldl (\(eph, per) element ->
                    let next_eph = RAL_eph.cons element eph in
                    let next_per = RAL_per.cons element per in
                    (next_eph, next_per)
            ) (RAL_eph.empty, RAL_per.empty) ins_elements
    in
    
    let upd_index_pureGen = mkStdGen upd_index_seed in
    let upd_indexes = rolls num (num - 1) upd_index_pureGen in
    
    let upd_elem_pureGen = mkStdGen upd_elem_seed in
    let upd_elements = rolls num (4 * num) upd_elem_pureGen in

    let (upd_eph_list, upd_per) =
            foldl (\(eph_h : eph_t, per) (index, element) ->
                    let next_eph = RAL_eph.update (index, element) eph_h in
                    let next_per = RAL_per.update (index, element) per in
                    (next_eph : eph_h : eph_t, next_per)
            ) ([ins_eph], ins_per) (zip upd_indexes upd_elements)
    in

    let build_persistent = build upd_per in
    
    let check_time_from = time ins_per - 1 in

    all (\(test_time, ephemeral_tree) ->
            ephemeral_tree == build_persistent test_time
    ) (zip [check_time_from .. check_time_from + num] (reverse upd_eph_list))


random_access_list_tail :: Int -> Int -> Bool
random_access_list_tail num ins_seed =
    let ins_pureGen = mkStdGen ins_seed in
    let ins_elements = random_shuffle num ins_pureGen in
    
    let (ins_eph, ins_per) =
            foldl (\(eph, per) element ->
                    let next_eph = RAL_eph.cons element eph in
                    let next_per = RAL_per.cons element per in
                    (next_eph, next_per)
            ) (RAL_eph.empty, RAL_per.empty) ins_elements
    in

    let (tail_eph_list, tail_per) =
            foldl (\(eph_h : eph_t, per) _ ->
                    let next_eph = RAL_eph.tail eph_h in
                    let next_per = RAL_per.tail per in
                    (next_eph : eph_h : eph_t, next_per)
            ) ([ins_eph], ins_per) [0 .. num - 1]
    in

    let build_persistent = build tail_per in
    
    let check_time_from = time ins_per - 1 in

    all (\(test_time, ephemeral_tree) ->
            ephemeral_tree == build_persistent test_time
    ) (zip [check_time_from .. check_time_from + num - 1] (reverse tail_eph_list))


random_access_list_update_final_element :: Int -> Int -> Int -> Bool
random_access_list_update_final_element num ins_seed upd_elem_seed =
    let ins_pureGen = mkStdGen ins_seed in
    let ins_elements = random_shuffle num ins_pureGen in

    let (ins_eph, ins_per) =
            foldl (\(eph, per) element ->
                    let next_eph = RAL_eph.cons element eph in
                    let next_per = RAL_per.cons element per in
                    (next_eph, next_per)
            ) (RAL_eph.empty, RAL_per.empty) ins_elements
    in
    
    let upd_elem_pureGen = mkStdGen upd_elem_seed in
    let upd_elements = rolls num (4 * num) upd_elem_pureGen in

    let (upd_eph_list, upd_per) =
            foldl (\(eph_h : eph_t, per) element ->
                    let next_eph = RAL_eph.update (num - 1, element) eph_h in
                    let next_per = RAL_per.update (num - 1, element) per in
                    (next_eph : eph_h : eph_t, next_per)
            ) ([ins_eph], ins_per) upd_elements
    in

    let build_persistent = build upd_per in
    
    let check_time_from = time ins_per - 1 in

    all (\(test_time, ephemeral_tree) ->
            ephemeral_tree == build_persistent test_time
    ) (zip [check_time_from .. check_time_from + num] (reverse upd_eph_list))
