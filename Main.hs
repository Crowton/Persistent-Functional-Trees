{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
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


size_test = do
    let size_start = 10
    let size_incr_mul = 1.3 :: Float
    let size_end = 1000

    let seed_start = 0
    let seed_end = 10

    putStrLn "n,tem,per"

    let size_loop size = do
        let seed_loop seed = do
            let (tem, per) = build_binary_tree size seed
            let per_root_list = build_root_list per

            tem_size <- recursiveSizeNF tem
            per_size <- recursiveSizeNF per_root_list

            putStrLn (show size ++ "," ++ show tem_size ++ "," ++ show per_size)

            when (seed < seed_end) (seed_loop (seed + 1))
        
        seed_loop seed_start

        when (size < size_end) (size_loop (ceiling ((fromIntegral size) * size_incr_mul)))

    size_loop size_start

    -- print ("Tem size: " ++ show tem_size ++ " bytes")
    -- print ("Per size: " ++ show per_size ++ " bytes")



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

    correctness_test
    -- size_test
