{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import Data.List as L

import Data.Function
import System.IO

import DataRecords

import Binary_Tree_temporal as TEM
import Binary_Tree_persistent_mock as PER_M
import Binary_Tree_persistent as PER

import DAG_construction

import Random_Test

import Prettify

import GHC.DataSize
import Control.Arrow
import Control.Monad

import Control.DeepSeq

import Control.Exception
-- import Formatting as F
import Formatting.Clock
import System.Clock

import System.Random
import qualified Text.XHtml as PER_M


small_temporal_tree_build = do
    let tree = Leaf
                & TEM.insert 3
                & TEM.insert 1
                & TEM.insert 2
                & TEM.insert 4
                & TEM.delete 3

    putStrLn (pretty_tree tree)
    putStrLn ("Contains 1: " ++ (show (TEM.contains 1 tree)))

small_mock_persistent_tree_build = do
    let persistent_tree =
            PER_M.construct_empty_tree
            & PER_M.insert 3
            & PER_M.insert 2
            & PER_M.insert 1
            & PER_M.insert 4
            & PER_M.delete 2
            & PER_M.delete 4

    let build_tree = build persistent_tree

    putStrLn ("Time 0:\n" ++ pretty_tree (build_tree 0) ++ "\n")
    putStrLn ("Time 1:\n" ++ pretty_tree (build_tree 1) ++ "\n")
    putStrLn ("Time 2:\n" ++ pretty_tree (build_tree 2) ++ "\n")
    putStrLn ("Time 3:\n" ++ pretty_tree (build_tree 3) ++ "\n")
    putStrLn ("Time 4:\n" ++ pretty_tree (build_tree 4) ++ "\n")
    putStrLn ("Time 5:\n" ++ pretty_tree (build_tree 5) ++ "\n")
    putStrLn ("Time 6:\n" ++ pretty_tree (build_tree 6) ++ "\n")
    putStrLn ("Time 7:\n" ++ pretty_tree (build_tree 7) ++ "\n")

small_persistent_tree_build = do
    let persistent_tree =
            PER.empty
            & PER.insert 3
            & PER.insert 3
            & PER.insert 2
            & PER.insert 1
            & PER.insert 4
            & PER.delete 2
            & PER.delete 4
            & PER.delete 5

    let build_tree = build persistent_tree

    -- putStrLn ("TimeTree\n" ++ pretty_time_tree (fst (head (rootList persistent_tree))) (currentTree persistent_tree) ++ "\n")

    putStrLn ("Time 0:\n" ++ pretty_tree (build_tree 0) ++ "\n")
    putStrLn ("Time 1:\n" ++ pretty_tree (build_tree 1) ++ "\n")
    putStrLn ("Time 2:\n" ++ pretty_tree (build_tree 2) ++ "\n")
    putStrLn ("Time 3:\n" ++ pretty_tree (build_tree 3) ++ "\n")
    putStrLn ("Time 4:\n" ++ pretty_tree (build_tree 4) ++ "\n")
    putStrLn ("Time 5:\n" ++ pretty_tree (build_tree 5) ++ "\n")
    putStrLn ("Time 6:\n" ++ pretty_tree (build_tree 6) ++ "\n")
    putStrLn ("Time 7:\n" ++ pretty_tree (build_tree 7) ++ "\n")
    putStrLn ("Time 8:\n" ++ pretty_tree (build_tree 8) ++ "\n")
    putStrLn ("Time 9:\n" ++ pretty_tree (build_tree 9) ++ "\n")



mock_correctness_test = do
    putStrLn "Running tests"

    -- Insertion unbalanced binary tree
    putStr "Insertion test ......... "
    hFlush stdout

    let binary_insertion_success = binary_tree_test_mock_insert 1000
    if binary_insertion_success
        then putStrLn "Success"
        else error "Test failed!"


    -- Deletion unbalanced binary tree
    putStr "Deletion test .......... "
    hFlush stdout

    let binary_deletion_success = binary_tree_test_mock_delete 1000
    if binary_deletion_success
        then putStrLn "Success"
        else error "Test failed!"


    -- Deletion unbalanced binary tree
    let size = 1000

    putStr "Node non splitting test .... "
    hFlush stdout

    let binary_split_success = binary_tree_mock_high_time_out_degree_node build_non_split size
    if binary_split_success
        then putStrLn "Success"
        else error "Test failed!"

    putStr "Node splitting test .... "
    hFlush stdout

    let binary_split_success = binary_tree_mock_high_time_out_degree_node build size
    if binary_split_success
        then putStrLn "Success"
        else error "Test failed!"


correctness_test = do
    putStrLn "Running tests"

    -- Insertion unbalanced binary tree
    putStr "Insertion test ......... "
    hFlush stdout

    let binary_insertion_success = binary_tree_test_insert 1000
    if binary_insertion_success
        then putStrLn "Success"
        else error "Test failed!"


    -- Deletion unbalanced binary tree
    putStr "Deletion test .......... "
    hFlush stdout

    let binary_deletion_success = binary_tree_test_delete 1000
    if binary_deletion_success
        then putStrLn "Success"
        else error "Test failed!"


    -- Deletion unbalanced binary tree
    let size = 1000

    putStr "Node non splitting test .... "
    hFlush stdout

    let binary_split_success = binary_tree_high_time_out_degree_node build_non_split size
    if binary_split_success
        then putStrLn "Success"
        else error "Test failed!"

    putStr "Node splitting test .... "
    hFlush stdout

    let binary_split_success = binary_tree_high_time_out_degree_node build size
    if binary_split_success
        then putStrLn "Success"
        else error "Test failed!"


delete_persistent_compare = do
    let build_elm = [2, 5, 8, 3, 9, 0, 1]
    let del_elm   = [0, 5, 3, 8, 1, 2, 9]

    let per_mock = foldl (\t e -> PER_M.insert e t) PER_M.construct_empty_tree build_elm
    let per_real = foldl (\t e -> PER.insert e t) PER.empty build_elm

    putStrLn ("Initial tree:\n" ++ (pretty_time_tree 1 (currentTree per_mock)) ++ "\n")

    let eq_per PartialTree {edgeFreezer=edgeFreezer1
                    , idStaticList=idStaticList1
                    , rootList=rootList1
                    , idCount=idCount1
                    , fieldCount=fieldCount1
                    , time=time1
                    , currentTree=currentTree1
                    }
               PartialTree {edgeFreezer=edgeFreezer2
                    , idStaticList=idStaticList2
                    , rootList=rootList2
                    , idCount=idCount2
                    , fieldCount=fieldCount2
                    , time=time2
                    , currentTree=currentTree2
                    } =
            (sort edgeFreezer1 == sort edgeFreezer2)
            && idStaticList1 == idStaticList2
            && (rootList1 == rootList2)
            && (idCount1 == idCount2)
            && (fieldCount1 == fieldCount2)
            && (time1 == time2)
            && (currentTree1 == currentTree2)


    let loop elements mock real = do
            let elm = head elements

            let new_mock = PER_M.delete elm mock
            let new_real = PER.delete elm real

            putStrLn ("After deleting " ++ show elm ++ ":")
            if eq_per new_mock new_real
                then putStrLn ("All good! The tree is now:\n" ++ (pretty_time_tree 1 (currentTree new_mock)) ++ "\n")
                else putStrLn ("Mock:\n" ++ show new_mock ++ "\n\n"
                            ++ "Real:\n" ++ show new_real ++ "\n")

            when ((tail elements) /= []) (loop (tail elements) new_mock new_real)

    loop del_elm per_mock per_real


temporal_tree_node_size_test = do
    let tree_0 = Leaf :: Tree Int
    let tree_1
            = tree_0
            & TEM.insert 1

    size_0 <- recursiveSizeNF tree_0
    size_1 <- recursiveSizeNF tree_1

    putStrLn ("Size of leaf: " ++ show size_0)
    putStrLn ("Delta size of single insert (0 -> 1): " ++ show (size_1 - size_0))

    let tree_2
            = tree_1
            & TEM.insert 2

    size_2 <- recursiveSizeNF tree_2

    putStrLn ("Delta size of single insert (1 -> 2): " ++ show (size_2 - size_1))

    let tree_7
            = tree_0
            & TEM.insert 4
            & TEM.insert 2
            & TEM.insert 6
            & TEM.insert 1
            & TEM.insert 3
            & TEM.insert 5
            & TEM.insert 7
    let tree_8
            = tree_7
            & TEM.insert 8

    size_7 <- recursiveSizeNF tree_7
    size_8 <- recursiveSizeNF tree_8

    putStrLn ("Delta size of single insert (7 -> 8): " ++ show (size_8 - size_7))

    let tree_9
            = tree_8
            & TEM.insert 0

    size_9 <- recursiveSizeNF tree_9

    putStrLn ("Delta size of single insert (8 -> 9): " ++ show (size_9 - size_8))

    putStrLn ("Size of tree 0: " ++ show size_0)
    putStrLn ("Size of tree 1: " ++ show size_1)
    putStrLn ("Size of tree 2: " ++ show size_2)
    putStrLn ("Size of tree 7: " ++ show size_7)
    putStrLn ("Size of tree 8: " ++ show size_8)
    putStrLn ("Size of tree 9: " ++ show size_9)


insertion_size_test builder = do
    let size_start = 10
    let size_incr_mul = 1.3 :: Float
    let size_end = 1000

    let seed_start = 0
    let seed_end = 30

    putStrLn "seed,n,tem,per"

    let size_loop size = do
        let seed_loop seed = do
            let (tem, per) = builder size seed
            let per_root_list = build_root_list per

            tem_size <- recursiveSizeNF tem
            per_size <- recursiveSizeNF per_root_list

            putStrLn (show seed ++ "," ++ show size ++ "," ++ show tem_size ++ "," ++ show per_size)
            hFlush stdout

            when (seed < seed_end - 1) (seed_loop (seed + 1))

        seed_loop seed_start

        when (size < size_end) (size_loop (ceiling ((fromIntegral size) * size_incr_mul)))

    size_loop size_start


deletion_size_test = do
    let size_start = 10
    let size_incr_mul = 1.3 :: Float
    let size_end = 4000

    putStrLn "n,per,splits"

    let size_loop size = do
        let per = build_mock_binary_persistent_tree_high_out_degree size
        let (per_root_list, node_splits) = build_root_list per

        per_size <- recursiveSizeNF per_root_list

        putStrLn (show size ++ "," ++ show per_size ++ "," ++ show node_splits)
        hFlush stdout

        when (size < size_end) (size_loop (ceiling ((fromIntegral size) * size_incr_mul)))

    size_loop size_start


deletion_size_range_test = do
    let size_start = 1
    let size_end = 1000

    putStrLn "n,per,splits"

    let size_loop size = do
        let per = build_mock_binary_persistent_tree_high_out_degree size
        let (per_root_list, node_splits) = build_root_list per

        per_size <- recursiveSizeNF per_root_list

        putStrLn (show size ++ "," ++ show per_size ++ "," ++ show node_splits)
        hFlush stdout

        when (size < size_end) (size_loop (size + 1))

    size_loop size_start


sanity_size_test = do
    -- let l = [] :: [Int]
    -- let l = [1::Int .. 100::Int]
    -- print (force l)
    -- size <- recursiveSize l
    -- size <- recursiveSizeNF l
    -- putStrLn (show size)

    let l = [] :: [Int]
    size <- recursiveSizeNF l
    putStrLn (show size)

    let l = [0] :: [Int]
    size <- recursiveSizeNF l
    putStrLn (show size)

    let l = [0, 1] :: [Int]
    size <- recursiveSizeNF l
    putStrLn (show size)

    let l = [0, 1, 2] :: [Int]
    size <- recursiveSizeNF l
    putStrLn (show size)

    let l = [0, 1, 2, 3] :: [Int]
    size <- recursiveSizeNF l
    putStrLn (show size)

    let l = [0, 1, 2, 3, 4] :: [Int]
    size <- recursiveSizeNF l
    putStrLn (show size)

    let l = [0, 1, 2, 3, 4, 5] :: [Int]
    size <- recursiveSizeNF l
    putStrLn (show size)

    let l = [0, 1, 2, 3, 4, 5, 6] :: [Int]
    size <- recursiveSizeNF l
    putStrLn (show size)

    let l = [0, 1, 2, 3, 4, 5, 6, 7] :: [Int]
    size <- recursiveSizeNF l
    putStrLn (show size)

    let l = [0, 1, 2, 3, 4, 5, 6, 7, 8] :: [Int]
    size <- recursiveSizeNF l
    putStrLn (show size)

    let l = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9] :: [Int]
    size <- recursiveSizeNF l
    putStrLn (show size)


    -- int_size <- recursiveSizeNF (1 :: Int)
    -- putStrLn (show int_size)

    -- int_size <- recursiveSizeNF (2147483647 :: Int)
    -- putStrLn (show int_size)

    -- int_size <- recursiveSizeNF (9223372036854775807 :: Int)
    -- putStrLn (show int_size)

    -- int_size <- recursiveSizeNF (9223372036854775807 :: Int)
    -- putStrLn (show int_size)


speed_test = do
    let (tem, per) = build_mock_binary_tree_without_duplicates 100000 10
    let !per_f = force per

    start <- getTime ProcessCPUTime

    let per_root_list = build_root_list per_f
    let !_ = force per_root_list

    end <- getTime ProcessCPUTime
    print (end - start)


update_insert_runtime_test = do
    let num = 1000
    let seed = 0

    let repeats = 10000000

    putStrLn ("repeats," ++ show repeats)
    putStrLn "n,tem,per"
    let loop n elements tem per = do
            let h = head elements

            let !tem_f = force tem

            start_tem <- getTime ProcessCPUTime
            let tem_loop itr = do
                let !new_tem = force (TEM.insert h tem_f)
                let itr' = itr + 1
                when (itr' < repeats) (tem_loop itr')
            tem_loop 0
            end_tem <- getTime ProcessCPUTime

            let !per_f = force per

            start_per <- getTime ProcessCPUTime
            let per_loop itr = do
                let !new_per = force (PER_M.insert h per_f)
                let itr' = itr + 1
                when (itr' < repeats) (per_loop itr')
            per_loop 0
            end_per <- getTime ProcessCPUTime

            putStrLn (show n ++ "," ++ show (toNanoSecs (end_tem - start_tem)) ++ "," ++ show (toNanoSecs (end_per - start_per)))
            hFlush stdout

            when ((tail elements) /= []) (loop (n + 1) (tail elements) (TEM.insert h tem_f) (PER_M.insert h per_f))


    let pureGen = mkStdGen seed
    let random_permutation = random_shuffle num pureGen
    loop 0 random_permutation Leaf PER_M.construct_empty_tree


update_insert_total_runtime_test = do
    let size_start = 10000
    let size_incr_mul = 1.3 :: Float
    let size_end = 1000000

    let seed_start = 0
    let seed_end = 30

    putStrLn "seed,n,tem,per"

    let size_loop size = do
        let seed_loop seed = do
            let pureGen = mkStdGen seed
            let !random_permutation = random_shuffle size pureGen

            start_tem <- getTime ProcessCPUTime
            let tem = foldl (flip TEM.insert) Leaf random_permutation
            let !tem_f = force tem
            end_tem <- getTime ProcessCPUTime

            start_per <- getTime ProcessCPUTime
            let per = foldl (flip PER_M.insert) PER_M.construct_empty_tree random_permutation
            let !per_f = force per
            end_per <- getTime ProcessCPUTime

            putStrLn (show seed ++ "," ++ show size ++ "," ++ show (toNanoSecs (end_tem - start_tem)) ++ "," ++ show (toNanoSecs (end_per - start_per)))
            hFlush stdout

            when (seed < seed_end - 1) (seed_loop (seed + 1))

        seed_loop seed_start

        when (size < size_end) (size_loop (ceiling ((fromIntegral size) * size_incr_mul)))

    size_loop size_start



main = do
    -- small_temporal_tree_build
    -- small_mock_persistent_tree_build
    -- small_persistent_tree_build
    -- delete_persistent_compare

    -- mock_correctness_test
    correctness_test
    -- speed_test
    -- sanity_size_test
    -- temporal_tree_node_size_test
    -- insertion_size_test build_binary_tree_without_duplicates
    -- deletion_size_test
    -- deletion_size_range_test

    -- update_insert_runtime_test
    -- update_insert_total_runtime_test

    -- let (tem, per) = build_binary_tree_without_duplicates 10 1
    -- let tree_10 : tem_rest = tem
    -- putStrLn (pretty_tree tree_10)
