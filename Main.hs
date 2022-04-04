module Main where

import Data.Function

import qualified Data.Map.Strict as MB

import DataRecords as D
import PartialTree_Create as Crea
import DAG_construction as C
-- import Query as Q
-- import Tree_Constructor as TC

import Binary_Tree_temporal as TEM
import Binary_Tree_persistent_mock as PER
import Prettify

import Random_Test

import Debug.Trace

import System.IO


run_tests = do
    putStrLn "Running test..."

    -- Insertion unbalanced binary tree
    putStr "Insertion test... "
    hFlush stdout

    let binary_insertion_success = binary_tree_test_insert 1000
    if binary_insertion_success
        then putStrLn "Success"
        else error "Test failed!"
    

    -- Insertion + delete unbalanced binary tree
    putStr "Deletion test... "
    hFlush stdout

    let binary_insertion_success = binary_tree_test_delete 1000
    if binary_insertion_success
        then putStrLn "Success"
        else error "Test failed!"


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
    
    -- putStrLn ("Time 0:\n" ++ (pretty_tree (build_tree 0)) ++ "\n")
    -- putStrLn ("Time 1:\n" ++ (pretty_tree (build_tree 1)) ++ "\n")
    -- putStrLn ("Time 2:\n" ++ (pretty_tree (build_tree 2)) ++ "\n")
    -- putStrLn ("Time 3:\n" ++ (pretty_tree (build_tree 3)) ++ "\n")
    -- putStrLn ("Time 4:\n" ++ (pretty_tree (build_tree 4)) ++ "\n")
    -- putStrLn ("Time 5:\n" ++ (pretty_tree (build_tree 5)) ++ "\n")
    -- putStrLn ("Time 6:\n" ++ (pretty_tree (build_tree 6)) ++ "\n")
    -- putStrLn ("Time 7:\n" ++ (pretty_tree (build_tree 7)) ++ "\n")

    run_tests
