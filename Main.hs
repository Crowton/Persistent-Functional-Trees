{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import Data.List as L

import Data.Function
import System.IO

import DataRecords

import Binary_Tree_ephemeral as EPH
import Binary_Tree_persistent_mock as PER_M
import Binary_Tree_persistent as PER

import RBTree_ephemeral as RB
import RBTree_persistent as RB_per

import qualified Random_access_list_ephemeral as RAL
import qualified Random_access_list_persistent as RAL_per

import Persistent_update
import DAG_construction
import Tree_Constructor

import qualified Data.Map.Strict as MB

import Random_Test

import Prettify

import GHC.DataSize
import Control.Monad

import Control.DeepSeq

import Control.Exception
import Formatting.Clock
import System.Clock

import Data.IORef
import Data.Time
import System.IO
import Control.Monad.IO.Class

import System.Random


-- Tests printing small trees to the terminal --

-- small_ephemeral_tree_build :: Show s => EPH_BST Int s -> IO ()
small_ephemeral_tree_build (eph_empty, eph_insert, eph_delete) eph_contains = do
    let tree =
            eph_empty
            & eph_insert 3
            & eph_insert 1
            & eph_insert 2
            & eph_insert 4
            & eph_delete 3

    putStrLn (pretty_tree tree)
    putStrLn ("Contains 1: " ++ (show (eph_contains 1 tree)) ++ "\n")

    let tree2 =
            eph_empty
            & eph_insert 1
            & eph_insert 2
            & eph_insert 3
            & eph_insert 4
            & eph_insert 5
            & eph_insert 6
            & eph_insert 7
            & eph_insert 8
            & eph_insert 9
            & eph_delete 4

    putStrLn (pretty_tree tree2)

small_persistent_tree_build (per_empty, per_insert, per_delete) = do
    let persistent_tree =
            per_empty
            & per_insert 3
            & per_insert 3
            & per_insert 2
            & per_insert 1
            & per_insert 4
            & per_delete 2
            & per_delete 4
            & per_delete 5

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

small_persistent_rotate = do
    let per_tree =
            PER.empty
            & PER.insert 6
            & PER.insert 2
            & PER.insert 7
            & PER.insert 1
            & PER.insert 4
            & PER.insert 3
            & PER.insert 5
            & update (\_ -> PER.rotate_right_left) Nothing

    let tree = build per_tree

    putStrLn ("Before rotation:\n" ++ pretty_tree (tree 7) ++ "\n")
    putStrLn ("After rotation:\n" ++ pretty_tree (tree 8) ++ "\n")

small_ephemeral_list_build = do
    let list =
            RAL.empty
            & RAL.cons 1
            & RAL.cons 2
            & RAL.cons 3
            & RAL.cons 4
            & RAL.cons 5
            & RAL.cons 6
            & RAL.cons 7
            & RAL.cons 8
            & RAL.cons 9
    
    putStrLn ((pretty_tree list) ++ "\n")

    putStrLn ("Head: " ++ show (RAL.head list) ++ "\n")
    putStrLn ("Lookup 5: " ++ show (RAL.lookup 5 list) ++ "\n")

    let list2 =
            list
            & RAL.update (7, 11)
    
    putStrLn ((pretty_tree list2) ++ "\n")

    let list3 =
            list2
            & RAL.tail
            & RAL.tail
            & RAL.tail
    
    putStrLn ((pretty_tree list3) ++ "\n")


-- Tests for correctness --

correctness_test eph_build per_build = do
    putStrLn "Running tests"
    hFlush stdout

    let test_run name test =
            do { putStr name
               ; hFlush stdout

               ; if test
                    then putStrLn "Success"
                    else error "Test failed!"
               }
        
    -- Insertion
    test_run
        "Insertion test ............ "
        (binary_tree_test_insert eph_build per_build 1000)

    -- Deletion
    test_run
        "Deletion test ............. "
        (binary_tree_test_delete eph_build per_build 1000)

    -- Deletion, which creates notes with high out degree
    let size = 1000
    test_run
        "Node non splitting test ... "
        (binary_tree_high_time_out_degree_node eph_build per_build build_non_split size)
    
    test_run
        "Node splitting test ....... "
        (binary_tree_high_time_out_degree_node eph_build per_build build size)

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

random_access_list_correctness = do
    putStrLn "Running tests"
    hFlush stdout

    let test_run name test =
            do { putStr name
               ; hFlush stdout

               ; if test
                    then putStrLn "Success"
                    else error "Test failed!"
               }
        
    -- Cons
    test_run
        "Cons test .......... "
        (random_access_list_cons 1000 42)

    -- Update
    test_run
        "Update test ........ "
        (random_access_list_update_uniform 1000 42 43 44)

    -- Tail
    test_run
        "Tail test .......... "
        (random_access_list_tail 5 42)

    -- Update in end
    test_run
        "Update last tast ... "
        (random_access_list_update_final_element 1000 42 43)


-- Checks for size of objects -- 

ephemeral_tree_node_size_test (eph_empty, eph_insert, _) = do
    let tree_0 = eph_empty :: Tree Int
    let tree_1
            = tree_0
            & eph_insert 1

    size_0 <- recursiveSizeNF tree_0
    size_1 <- recursiveSizeNF tree_1

    putStrLn ("Size of leaf: " ++ show size_0)
    putStrLn ("Delta size of single insert (0 -> 1): " ++ show (size_1 - size_0))

    let tree_2
            = tree_1
            & eph_insert 2

    size_2 <- recursiveSizeNF tree_2

    putStrLn ("Delta size of single insert (1 -> 2): " ++ show (size_2 - size_1))

    let tree_7
            = tree_0
            & eph_insert 4
            & eph_insert 2
            & eph_insert 6
            & eph_insert 1
            & eph_insert 3
            & eph_insert 5
            & eph_insert 7
    let tree_8
            = tree_7
            & eph_insert 8

    size_7 <- recursiveSizeNF tree_7
    size_8 <- recursiveSizeNF tree_8

    putStrLn ("Delta size of single insert (7 -> 8): " ++ show (size_8 - size_7))

    let tree_9
            = tree_8
            & eph_insert 0

    size_9 <- recursiveSizeNF tree_9

    putStrLn ("Delta size of single insert (8 -> 9): " ++ show (size_9 - size_8))

    putStrLn ("Size of tree 0: " ++ show size_0)
    putStrLn ("Size of tree 1: " ++ show size_1)
    putStrLn ("Size of tree 2: " ++ show size_2)
    putStrLn ("Size of tree 7: " ++ show size_7)
    putStrLn ("Size of tree 8: " ++ show size_8)
    putStrLn ("Size of tree 9: " ++ show size_9)

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


-- Tests for size of build dag --

size_compare_test builder = do
    let size_start = 10
    let size_incr_mul = 1.3 :: Float
    let size_end = 1000

    let seed_start = 0
    let seed_end = 30

    putStrLn "seed,n,eph,per"

    let size_loop size = do
        let seed_loop seed = do
            let (eph, per) = builder size seed
            let (per_root_list, _) = build_root_list per

            eph_size <- recursiveSizeNF eph
            per_size <- recursiveSizeNF per_root_list

            putStrLn (show seed ++ "," ++ show size ++ "," ++ show eph_size ++ "," ++ show per_size)
            hFlush stdout

            when (seed + 1 < seed_end) (seed_loop (seed + 1))

        seed_loop seed_start

        when (size < size_end) (size_loop (ceiling ((fromIntegral size) * size_incr_mul)))

    size_loop size_start

size_worst_case_compare_test (eph_empty, eph_insert, eph_delete) (per_empty, per_insert, per_delete) = do
    let size_start = 10
    let size_incr_mul = 1.3 :: Float
    let size_end = 10000

    putStrLn "n,eph,per,splits"

    let size_loop size = do
        -- TRUE n = 3 * n
        let (eph_base, per_base) = foldl (\(eph_h : eph_t, per) element ->
                                            let next_eph = eph_insert element eph_h in
                                            let next_per = per_insert element per in
                                            (next_eph : eph_h : eph_t, next_per)
                                   ) ([eph_empty], per_empty) [1 :: Int .. size]
        
        let (eph_final, per_final) = foldl (\(eph_h : eph_t, per) _ ->
                                              let next_eph = eph_insert (size + 1) eph_h in
                                              let next_next_eph = eph_delete (size + 1) next_eph in
                                              let next_per = per_delete (size + 1) (per_insert (size + 1) per) in
                                              (next_next_eph : next_eph : eph_h : eph_t, next_per)
                                     ) (eph_base, per_base) [1 :: Int .. size]

        let (per_root_list, splits) = build_root_list per_final

        eph_size <- recursiveSizeNF eph_final
        per_size <- recursiveSizeNF per_root_list

        putStrLn (show size ++ "," ++ show eph_size ++ "," ++ show per_size ++ "," ++ show splits)
        hFlush stdout

        when (size < size_end) (size_loop (ceiling ((fromIntegral size) * size_incr_mul)))

    size_loop size_start


size_worst_case_test (per_empty, per_insert, per_delete) = do
    let size_start = 10
    let size_incr_mul = 1.3 :: Float
    let size_end = 10000000

    putStrLn "n,per,splits"

    let size_loop size = do
        -- TRUE n = 3 * n
        let per_base = foldl (flip per_insert) per_empty [1 :: Int .. size]
        let per = foldl (\p _ -> per_delete (size + 1) (per_insert (size + 1) p)) per_base [1 .. size]

        let (per_root_list, splits) = build_root_list per

        per_size <- recursiveSizeNF per_root_list

        putStrLn (show size ++ "," ++ show per_size ++ "," ++ show splits)
        hFlush stdout

        when (size < size_end) (size_loop (ceiling ((fromIntegral size) * size_incr_mul)))

    size_loop size_start


size_worst_case_range_test (per_empty, per_insert, per_delete) = do
    let size_start = 1
    let size_end = 10000

    putStrLn "n,per,splits"

    let size_loop size = do
        -- TRUE n = 3 * n
        let per_base = foldl (flip per_insert) per_empty [1 :: Int .. size]
        let per = foldl (\p _ -> per_delete (size + 1) (per_insert (size + 1) p)) per_base [1 .. size]

        let (per_root_list, splits) = build_root_list per

        per_size <- recursiveSizeNF per_root_list

        putStrLn (show size ++ "," ++ show per_size ++ "," ++ show splits)
        hFlush stdout

        when (size + 1 < size_end) (size_loop (size + 1))

    size_loop size_start


-- Tests for run time --

sanity_runtime_check = do
    let size_start = 1000
    let size_incr_mul = 1.3 :: Float
    let size_end = 100000000

    let seed_start = 0
    let seed_end = 30

    let repeats = 10

    putStrLn "seed,n,time"

    let size_loop size = do
        let seed_loop seed = do
            -- TODO: move this to generator code space
            let eph_builder :: Ord e => [e] -> Tree e -> Tree e
                eph_builder elements tree =
                    case elements of
                        [] -> tree
                        _  -> let (left, mid : right) = splitAt ((length elements) `div` 2) elements in
                              eph_builder right (eph_builder left (EPH.insert mid tree))
            
            let eph = eph_builder [1 .. size] Leaf
            let !eph_f = force eph

            let pureGen = mkStdGen seed
            let query_elements = random_shuffle size pureGen
            let !query_elements_f = force query_elements

            let repeat_loop itr = do
                start <- liftIO getCurrentTime

                let query_loop elms = do
                    let q = head elms
                    
                    let res = EPH.contains q eph_f
                    let !res_f = force res

                    when ((tail elms) /= []) (query_loop (tail elms))
                
                query_loop query_elements_f

                -- TODO: is this better? Or is there allocation issues?
                -- let res = map (\elm -> EPH.contains elm eph_f) query_elements_f
                -- let !res_f = force res

                end <- liftIO getCurrentTime

                let elapsedTime = realToFrac $ end `diffUTCTime` start

                putStrLn (show seed ++ "," ++ show size ++ "," ++ show elapsedTime)
                hFlush stdout

                when (itr + 1 < repeats) (repeat_loop (itr + 1))

            repeat_loop 0

            when (seed < seed_end - 1) (seed_loop (seed + 1))

        seed_loop seed_start

        when (size < size_end) (size_loop (ceiling ((fromIntegral size) * size_incr_mul)))

    size_loop size_start


-- TODO: similar??
update_insert_total_runtime_test (eph_empty, eph_insert, _) (per_empty, per_insert, _) = do
    -- TODO: scale seed and repeats automaically
    let size_start = 48269 -- 10000
    let size_incr_mul = 1.3 :: Float
    let size_end = 1000000

    let seed_start = 0
    let seed_end = 15 -- 30

    let repeats = 2  -- 10

    putStrLn "seed,n,eph,per"

    let size_loop size = do
        let seed_loop seed = do
            let repeat_loop itr = do
                let pureGen = mkStdGen seed
                let !random_permutation = random_shuffle size pureGen

                start_eph <- liftIO getCurrentTime
                let eph = foldl (flip eph_insert) eph_empty random_permutation
                let !eph_f = force eph
                end_eph <- liftIO getCurrentTime
                let elapsedTime_eph = realToFrac $ end_eph `diffUTCTime` start_eph

                start_per <- liftIO getCurrentTime
                let per = foldl (flip per_insert) per_empty random_permutation
                let !per_f = force per
                end_per <- liftIO getCurrentTime
                let elapsedTime_per = realToFrac $ end_per `diffUTCTime` start_per

                putStrLn (show seed ++ "," ++ show size ++ "," ++ show elapsedTime_eph ++ "," ++ show elapsedTime_per)
                hFlush stdout

                when (itr + 1 < repeats) (repeat_loop (itr + 1))

            repeat_loop 0

            when (seed < seed_end - 1) (seed_loop (seed + 1))

        seed_loop seed_start

        when (size < size_end) (size_loop (ceiling ((fromIntegral size) * size_incr_mul)))

    size_loop size_start

update_insert_range_total_runtime_test (eph_empty, eph_insert, _) (per_empty, per_insert, _) = do
    let size_start = 1000
    let size_incr_mul = 1.3 :: Float
    let size_end = 10000000

    let repeats = 20

    putStrLn "n,eph,per"

    let size_loop size = do
        let repeat_loop itr = do
            let !elements = [1 :: Int .. size]

            start_eph <- liftIO getCurrentTime
            let eph = foldl (flip eph_insert) eph_empty elements
            let !eph_f = force eph
            end_eph <- liftIO getCurrentTime
            let elapsedTime_eph = realToFrac $ end_eph `diffUTCTime` start_eph

            start_per <- liftIO getCurrentTime
            let per = foldl (flip per_insert) per_empty elements
            let !per_f = force per
            end_per <- liftIO getCurrentTime
            let elapsedTime_per = realToFrac $ end_per `diffUTCTime` start_per

            putStrLn (show size ++ "," ++ show elapsedTime_eph ++ "," ++ show elapsedTime_per)
            hFlush stdout

            when (itr + 1 < repeats) (repeat_loop (itr + 1))

        repeat_loop 0

        when (size < size_end) (size_loop (ceiling ((fromIntegral size) * size_incr_mul)))

    size_loop size_start

update_insert_and_delete_total_runtime_test (eph_empty, eph_insert, eph_delete) (per_empty, per_insert, per_delete) = do
    -- TODO: scale seed and repeats automaically
    let size_start = 10000
    let size_incr_mul = 1.3 :: Float
    let size_end = 1000000

    let seed_start = 0
    -- let seed_end = 30

    -- let repeats = 10

    putStrLn "seed,n,eph,per"

    let size_loop size = do
        let seed_end = if size < 25000 then 30 else 15
        let repeats = if size < 25000 then 10 else 2

        let seed_loop seed = do
            let repeat_loop itr = do
                let pureGen = mkStdGen seed
                let !random_insert_permutation = random_shuffle size pureGen

                let pureGen = mkStdGen (-seed)
                let !random_delete_permutation = random_shuffle size pureGen

                start_eph <- liftIO getCurrentTime
                let eph_base = foldl (flip eph_insert) eph_empty random_insert_permutation
                let eph = foldl (flip eph_delete) eph_base random_delete_permutation
                let !eph_f = force eph
                end_eph <- liftIO getCurrentTime
                let elapsedTime_eph = realToFrac $ end_eph `diffUTCTime` start_eph

                start_per <- liftIO getCurrentTime
                let per_base = foldl (flip per_insert) per_empty random_insert_permutation
                let per = foldl (flip per_delete) per_base random_delete_permutation
                let !per_f = force per
                end_per <- liftIO getCurrentTime
                let elapsedTime_per = realToFrac $ end_per `diffUTCTime` start_per

                putStrLn (show seed ++ "," ++ show size ++ "," ++ show elapsedTime_eph ++ "," ++ show elapsedTime_per)
                hFlush stdout

                when (itr + 1 < repeats) (repeat_loop (itr + 1))

            repeat_loop 0

            when (seed < seed_end - 1) (seed_loop (seed + 1))

        seed_loop seed_start

        when (size < size_end) (size_loop (ceiling ((fromIntegral size) * size_incr_mul)))

    size_loop size_start

-- TODO: refracter to take builder function
dag_build_insert_only_speed_test (per_empty, per_insert, per_delete) = do
    let size_start = 500
    let size_incr_mul = 1.3 :: Float
    let size_end = 20000000

    let seed_start = 0
    -- let seed_end = 30

    -- let repeats = 10

    putStrLn "seed,n,time"

    let size_loop size = do
        let seed_end = if size < 10000 then 30 else 15
        let repeats = if size < 10000 then 10 else 2

        let seed_loop seed = do
            -- TODO: this is similar to build and destroy method, refractor
            -- TODO: Move to random test file
            let pureGen = mkStdGen seed
            let random_permutation = random_shuffle size pureGen

            let per = foldl (flip per_insert) per_empty random_permutation
            let !per_f = force per

            let repeat_loop itr = do
                -- Record building step
                start <- liftIO getCurrentTime
                let (root_list, splits) = build_root_list per_f
                let !root_list_f = force root_list
                let !splits_f = force splits
                end <- liftIO getCurrentTime

                let elapsedTime = realToFrac $ end `diffUTCTime` start

                putStrLn (show seed ++ "," ++ show size ++ "," ++ show elapsedTime ++ "," ++ show splits_f)
                hFlush stdout

                when (itr + 1 < repeats) (repeat_loop (itr + 1))

            repeat_loop 0

            when (seed < seed_end - 1) (seed_loop (seed + 1))

        seed_loop seed_start

        when (size < size_end) (size_loop (ceiling ((fromIntegral size) * size_incr_mul)))

    size_loop size_start

dag_build_insert_delete_speed_test (per_empty, per_insert, per_delete) = do
    let size_start = 500
    let size_incr_mul = 1.3 :: Float
    let size_end = 20000

    let seed_start = 0
    -- let seed_end = 30

    -- let repeats = 10

    putStrLn "seed,n,time"

    let size_loop size = do
        let seed_end = if size < 5000 then 30 else 15
        let repeats = if size < 5000 then 10 else 2

        let seed_loop seed = do
            -- TODO: this is similar to build and destroy method, refractor
            -- TODO: Move to random test file
            let pureGen = mkStdGen seed
            let random_permutation_insert = random_shuffle size pureGen

            let pureGen = mkStdGen (-seed)
            let random_permutation_delete = random_shuffle size pureGen

            let per = foldl (flip per_delete) (foldl (flip per_insert) per_empty random_permutation_insert) random_permutation_delete
            let !per_f = force per

            let repeat_loop itr = do
                -- Record building step
                start <- liftIO getCurrentTime
                let (root_list, splits) = build_root_list per_f
                let !root_list_f = force root_list
                let !splits_f = force splits
                end <- liftIO getCurrentTime

                let elapsedTime = realToFrac $ end `diffUTCTime` start

                putStrLn (show seed ++ "," ++ show size ++ "," ++ show elapsedTime ++ "," ++ show splits_f)
                hFlush stdout

                when (itr + 1 < repeats) (repeat_loop (itr + 1))

            repeat_loop 0

            when (seed + 1 < seed_end) (seed_loop (seed + 1))

        seed_loop seed_start

        when (size < size_end) (size_loop (ceiling ((fromIntegral size) * size_incr_mul)))

    size_loop size_start

dag_build_worst_case_delete_speed_test (per_empty, per_insert, per_delete) = do
    let size_start = 500
    let size_incr_mul = 1.3 :: Float
    let size_end = 4000

    let repeats = 30

    putStrLn "n,time,splits"

    let size_loop size = do
        -- TRUE n = 3 * n
        let per_base = foldl (flip per_insert) per_empty [1 :: Int .. size]
        let per = foldl (\p _ -> per_delete (size + 1) (per_insert (size + 1) p)) per_base [1 .. size]
        let !per_f = force per

        let repeat_loop itr = do
            -- Record building step
            start <- liftIO getCurrentTime
            let (root_list, splits) = build_root_list per_f
            let !root_list_f = force root_list
            let !splits_f = force splits
            end <- liftIO getCurrentTime

            let elapsedTime = realToFrac $ end `diffUTCTime` start

            putStrLn (show size ++ "," ++ show elapsedTime ++ "," ++ show splits_f)
            hFlush stdout

            when (itr + 1 < repeats) (repeat_loop (itr + 1))

        repeat_loop 0

        when (size < size_end) (size_loop (ceiling ((fromIntegral size) * size_incr_mul)))

    size_loop size_start


query_only_inserts_fixed_size_sum_elements_runtime_test (eph_empty, eph_insert, _) (per_empty, per_insert, _) = do
    let time_start = 1000
    let time_incr_mul = 1.3 :: Float
    
    let size = 200000
    
    let seed_start = 0
    let seed_end = 40

    let repeats = 20

    putStrLn "seed,version,eph,per"

    let seed_loop seed = do
        let pureGen = mkStdGen seed
        let !random_permutation = random_shuffle size pureGen

        let (_ : eph_list, _, times) = foldl 
                (\(eph_h : eph_t, next_time, times) (time, elm) ->
                    let new_eph = eph_insert elm eph_h in
                    if (time == next_time)
                        then let new_next_time = ceiling ((fromIntegral next_time) * time_incr_mul) in
                             (new_eph : new_eph : eph_t, new_next_time, time : times)
                        else (new_eph : eph_t, next_time, times)
                )
                ([eph_empty], time_start, [])
                (zip [1..] random_permutation)
        
        let !eph_list_f = force (reverse eph_list)
        let !times_f = force (reverse times)

        let per = foldl (flip per_insert) per_empty random_permutation
        let (rootNodeList, _) = build_root_list per
        let rootMap = MB.fromDistinctDescList rootNodeList
        let !rootMap_f = force rootMap

        let per_tree = construct (fieldCount per) rootMap


        let time_loop times ephs = do
            let !time = force (head times)
            let !eph = force (head ephs)
            
            let repeat_loop itr = do
                start_eph <- liftIO getCurrentTime
                let eph_sum = EPH.sum eph
                let !eph_f = force eph_sum
                end_eph <- liftIO getCurrentTime
                let elapsedTime_eph = realToFrac $ end_eph `diffUTCTime` start_eph

                start_per <- liftIO getCurrentTime
                let per_sum = EPH.sum (per_tree time)
                let !per_f = force per_sum
                end_per <- liftIO getCurrentTime
                let elapsedTime_per = realToFrac $ end_per `diffUTCTime` start_per

                putStrLn (show seed ++ "," ++ show time ++ "," ++ show elapsedTime_eph ++ "," ++ show elapsedTime_per)
                hFlush stdout

                when (itr + 1 < repeats) (repeat_loop (itr + 1))

            repeat_loop 0

            when (tail times /= []) (time_loop (tail times) (tail ephs))
        
        time_loop times_f eph_list_f

        when (seed + 1 < seed_end) (seed_loop (seed + 1))

    seed_loop seed_start


query_only_inserts_range_fixed_size_sum_elements_runtime_test (eph_empty, eph_insert, _) (per_empty, per_insert, _) = do
    let time_start = 1000
    let time_incr_mul = 1.3 :: Float
    
    let size = 100000

    let repeats = 30

    putStrLn "version,eph,per"

    let !elements = [1::Int .. size]

    let (_ : eph_list, _, times) = foldl 
            (\(eph_h : eph_t, next_time, times) (time, elm) ->
                let new_eph = eph_insert elm eph_h in
                if (time == next_time)
                    then let new_next_time = ceiling ((fromIntegral next_time) * time_incr_mul) in
                            (new_eph : new_eph : eph_t, new_next_time, time : times)
                    else (new_eph : eph_t, next_time, times)
            )
            ([eph_empty], time_start, [])
            (zip [1..] elements)
    
    let !eph_list_f = force (reverse eph_list)
    let !times_f = force (reverse times)

    let per = foldl (flip per_insert) per_empty elements
    let (rootNodeList, _) = build_root_list per
    let rootMap = MB.fromDistinctDescList rootNodeList
    let !rootMap_f = force rootMap

    let per_tree = construct (fieldCount per) rootMap


    let time_loop times ephs = do
        let !time = force (head times)
        let !eph = force (head ephs)
        
        let repeat_loop itr = do
            start_eph <- liftIO getCurrentTime
            let eph_sum = RB.sum eph
            let !eph_f = force eph_sum
            end_eph <- liftIO getCurrentTime
            let elapsedTime_eph = realToFrac $ end_eph `diffUTCTime` start_eph

            start_per <- liftIO getCurrentTime
            let per_sum = RB.sum (per_tree time)
            let !per_f = force per_sum
            end_per <- liftIO getCurrentTime
            let elapsedTime_per = realToFrac $ end_per `diffUTCTime` start_per

            putStrLn (show time ++ "," ++ show elapsedTime_eph ++ "," ++ show elapsedTime_per)
            hFlush stdout

            when (itr + 1 < repeats) (repeat_loop (itr + 1))

        repeat_loop 0

        when (tail times /= []) (time_loop (tail times) (tail ephs))
    
    time_loop times_f eph_list_f


query_worst_case_insert_delete_fixed_size_contains_low_leaf_runtime_test (eph_empty, eph_insert, _) (per_empty, per_insert, per_delete) = do
    let time_start = 10
    let time_incr_mul = 1.2 :: Float
    
    let size = 3000

    let time_end = 3 * size
    let real_time_start = time_start + size
    
    let repeats = 30

    putStrLn "version,eph,per"


    let eph_leaf = foldl (flip eph_insert) eph_empty [1 :: Int .. size]
    let eph_node = eph_insert (size + 1) eph_leaf

    let per_base = foldl (flip per_insert) per_empty [1 :: Int .. size]
    let per = foldl (\p _ -> per_delete (size + 1) (per_insert (size + 1) p)) per_base [1 .. size]

    let (rootNodeList, _) = build_root_list per
    let rootMap = MB.fromDistinctDescList rootNodeList
    let !rootMap_f = force rootMap

    let per_tree = construct (fieldCount per) rootMap
    
    let time_loop time = do
        let !eph = force (if (EPH.contains (size + 1) (per_tree time)) then eph_node else eph_leaf) 

        let repeat_loop itr = do
            start_eph <- liftIO getCurrentTime
            let eph_res = EPH.contains (size + 1) eph
            let !eph_f = force eph_res
            end_eph <- liftIO getCurrentTime
            let elapsedTime_eph = realToFrac $ end_eph `diffUTCTime` start_eph

            start_per <- liftIO getCurrentTime
            let per_res = EPH.contains (size + 1) (per_tree time)
            let !per_f = force per_res
            end_per <- liftIO getCurrentTime
            let elapsedTime_per = realToFrac $ end_per `diffUTCTime` start_per

            putStrLn (show time ++ "," ++ show elapsedTime_eph ++ "," ++ show elapsedTime_per)
            hFlush stdout

            when (itr + 1 < repeats) (repeat_loop (itr + 1))

        repeat_loop 0

        let new_time = ceiling ((fromIntegral time) * time_incr_mul)

        when (new_time < time_end) (time_loop new_time)
    
    time_loop real_time_start


main = do
    -- small_ephemeral_tree_build EPH.get_func EPH.contains
    -- small_ephemeral_tree_build RB.get_func RB.member
    -- small_persistent_tree_build PER_M.get_func
    -- small_persistent_tree_build PER.get_func
    -- small_persistent_rotate
    -- small_ephemeral_list_build
    
    -- correctness_test EPH.get_func PER.get_func
    -- correctness_test RB.get_func RB_per.get_func
    -- delete_persistent_compare
    -- random_access_list_correctness

    -- ephemeral_tree_node_size_test EPH.get_func
    -- sanity_size_test

    -- size_compare_test (build_binary_tree_without_duplicates EPH.get_func PER.get_func)
    -- size_compare_test (build_and_destroy_binary_tree_without_duplicates EPH.get_func PER.get_func)
    -- size_worst_case_compare_test EPH.get_func PER.get_func
    -- size_worst_case_test PER.get_func
    -- size_worst_case_range_test PER.get_func

    -- update_insert_total_runtime_test EPH.get_func PER.get_func
    -- update_insert_and_delete_total_runtime_test EPH.get_func PER.get_func

    -- sanity_runtime_check
    -- dag_build_insert_only_speed_test PER.get_func  -- TODO: this one!
    -- dag_build_insert_delete_speed_test PER.get_func
    -- dag_build_worst_case_delete_speed_test PER.get_func

    -- query_only_inserts_fixed_size_sum_elements_runtime_test EPH.get_func PER.get_func
    query_only_inserts_range_fixed_size_sum_elements_runtime_test RB.get_func RB_per.get_func
    -- query_worst_case_insert_delete_fixed_size_contains_low_leaf_runtime_test EPH.get_func PER.get_func

    -- update_insert_range_total_runtime_test RB.get_func RB_per.get_func

    -- let (eph, per) = build_binary_tree_without_duplicates EPH.get_func PER.get_func 10 1
    -- let tree_10 : eph_rest = eph
    -- putStrLn (pretty_tree tree_10)
