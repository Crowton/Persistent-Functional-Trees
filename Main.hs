{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import Data.Function
import System.IO

import DataRecords

import Binary_Tree_temporal as TEM
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

            when (seed < seed_end - 1) (seed_loop (seed + 1))

        seed_loop seed_start

        when (size < size_end) (size_loop (ceiling ((fromIntegral size) * size_incr_mul)))

    size_loop size_start

    -- print ("Tem size: " ++ show tem_size ++ " bytes")
    -- print ("Per size: " ++ show per_size ++ " bytes")


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
    let (tem, per) = build_binary_tree_without_duplicates 100000 10
    let !per_f = force per

    start <- getTime ProcessCPUTime
    let per_root_list = build_root_list per_f
    let !_ = force per_root_list

    end <- getTime ProcessCPUTime
    -- fprint (timeSpecs % "\n") start end

    print (end - start)


    -- start_time <- getSystemTime

    -- let (tem, per) = build_binary_tree_without_duplicates 1000 10
    -- let per_root_list = build_root_list per

    -- end_time <- getSystemTime

    -- print (end_time - start_time)


main = do
    let tree = Leaf
                & TEM.insert 3
                & TEM.insert 1
                & TEM.insert 2
                & TEM.insert 4
                & TEM.delete 3

    -- putStrLn (pretty_tree tree)
    -- print (TEM.contains 1 tree)

    let persistent_tree =
            PER.construct_empty_tree
            & PER.insert 3
            & PER.insert 2
            & PER.insert 1
            & PER.insert 4
            & PER.delete 2
            & PER.delete 4

    let build_tree = build persistent_tree

    -- putStrLn ("Time 0:\n" ++ pretty_tree (build_tree 0) ++ "\n")
    -- putStrLn ("Time 1:\n" ++ pretty_tree (build_tree 1) ++ "\n")
    -- putStrLn ("Time 2:\n" ++ pretty_tree (build_tree 2) ++ "\n")
    -- putStrLn ("Time 3:\n" ++ pretty_tree (build_tree 3) ++ "\n")
    -- putStrLn ("Time 4:\n" ++ pretty_tree (build_tree 4) ++ "\n")
    -- putStrLn ("Time 5:\n" ++ pretty_tree (build_tree 5) ++ "\n")
    -- putStrLn ("Time 6:\n" ++ pretty_tree (build_tree 6) ++ "\n")
    -- putStrLn ("Time 7:\n" ++ pretty_tree (build_tree 7) ++ "\n")

    -- correctness_test
    -- speed_test
    -- sanity_size_test
    -- temporal_tree_node_size_test
    insertion_size_test build_binary_tree_without_duplicates

    -- let (tem, per) = build_binary_tree_without_duplicates 10 1
    -- let tree_10 : tem_rest = tem
    -- putStrLn (pretty_tree tree_10)
