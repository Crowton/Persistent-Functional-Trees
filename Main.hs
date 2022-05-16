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

import RBTree_temporal as RB
import RBTree_persistent as RB_per

import qualified Random_access_list_temporal as RAL
import qualified Random_access_list_persistent as RAL_per

import Persistent_update
import DAG_construction

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

-- small_temporal_tree_build :: Show s => TEM_BST Int s -> IO ()
small_temporal_tree_build (tem_empty, tem_insert, tem_delete) tem_contains = do
    let tree =
            tem_empty
            & tem_insert 3
            & tem_insert 1
            & tem_insert 2
            & tem_insert 4
            & tem_delete 3

    putStrLn (pretty_tree tree)
    putStrLn ("Contains 1: " ++ (show (tem_contains 1 tree)) ++ "\n")

    let tree2 =
            tem_empty
            & tem_insert 1
            & tem_insert 2
            & tem_insert 3
            & tem_insert 4
            & tem_insert 5
            & tem_insert 6
            & tem_insert 7
            & tem_insert 8
            & tem_insert 9
            & tem_delete 4

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

small_temporal_list_build = do
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

correctness_test tem_build per_build = do
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
        (binary_tree_test_insert tem_build per_build 1000)

    -- Deletion
    test_run
        "Deletion test ............. "
        (binary_tree_test_delete tem_build per_build 1000)

    -- Deletion, which creates notes with high out degree
    let size = 1000
    test_run
        "Node non splitting test ... "
        (binary_tree_high_time_out_degree_node tem_build per_build build_non_split size)
    
    test_run
        "Node splitting test ....... "
        (binary_tree_high_time_out_degree_node tem_build per_build build size)

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

temporal_tree_node_size_test (tem_empty, tem_insert, _) = do
    let tree_0 = tem_empty :: Tree Int
    let tree_1
            = tree_0
            & tem_insert 1

    size_0 <- recursiveSizeNF tree_0
    size_1 <- recursiveSizeNF tree_1

    putStrLn ("Size of leaf: " ++ show size_0)
    putStrLn ("Delta size of single insert (0 -> 1): " ++ show (size_1 - size_0))

    let tree_2
            = tree_1
            & tem_insert 2

    size_2 <- recursiveSizeNF tree_2

    putStrLn ("Delta size of single insert (1 -> 2): " ++ show (size_2 - size_1))

    let tree_7
            = tree_0
            & tem_insert 4
            & tem_insert 2
            & tem_insert 6
            & tem_insert 1
            & tem_insert 3
            & tem_insert 5
            & tem_insert 7
    let tree_8
            = tree_7
            & tem_insert 8

    size_7 <- recursiveSizeNF tree_7
    size_8 <- recursiveSizeNF tree_8

    putStrLn ("Delta size of single insert (7 -> 8): " ++ show (size_8 - size_7))

    let tree_9
            = tree_8
            & tem_insert 0

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

    putStrLn "seed,n,tem,per"

    let size_loop size = do
        let seed_loop seed = do
            let (tem, per) = builder size seed
            let (per_root_list, _) = build_root_list per

            tem_size <- recursiveSizeNF tem
            per_size <- recursiveSizeNF per_root_list

            putStrLn (show seed ++ "," ++ show size ++ "," ++ show tem_size ++ "," ++ show per_size)
            hFlush stdout

            when (seed + 1 < seed_end) (seed_loop (seed + 1))

        seed_loop seed_start

        when (size < size_end) (size_loop (ceiling ((fromIntegral size) * size_incr_mul)))

    size_loop size_start

size_worst_case_compare_test (tem_empty, tem_insert, tem_delete) (per_empty, per_insert, per_delete) = do
    let size_start = 10
    let size_incr_mul = 1.3 :: Float
    let size_end = 10000

    putStrLn "n,tem,per,splits"

    let size_loop size = do
        -- TRUE n = 3 * n
        let (tem_base, per_base) = foldl (\(tem_h : tem_t, per) element ->
                                            let next_tem = tem_insert element tem_h in
                                            let next_per = per_insert element per in
                                            (next_tem : tem_h : tem_t, next_per)
                                   ) ([tem_empty], per_empty) [1 :: Int .. size]
        
        let (tem_final, per_final) = foldl (\(tem_h : tem_t, per) _ ->
                                              let next_tem = tem_insert (size + 1) tem_h in
                                              let next_next_tem = tem_delete (size + 1) next_tem in
                                              let next_per = per_delete (size + 1) (per_insert (size + 1) per) in
                                              (next_next_tem : next_tem : tem_h : tem_t, next_per)
                                     ) (tem_base, per_base) [1 :: Int .. size]

        let (per_root_list, splits) = build_root_list per_final

        tem_size <- recursiveSizeNF tem_final
        per_size <- recursiveSizeNF per_root_list

        putStrLn (show size ++ "," ++ show tem_size ++ "," ++ show per_size ++ "," ++ show splits)
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
            let tem_builder :: Ord e => [e] -> Tree e -> Tree e
                tem_builder elements tree =
                    case elements of
                        [] -> tree
                        _  -> let (left, mid : right) = splitAt ((length elements) `div` 2) elements in
                              tem_builder right (tem_builder left (TEM.insert mid tree))
            
            let tem = tem_builder [1 .. size] Leaf
            let !tem_f = force tem

            let pureGen = mkStdGen seed
            let query_elements = random_shuffle size pureGen
            let !query_elements_f = force query_elements

            let repeat_loop itr = do
                start <- liftIO getCurrentTime

                let query_loop elms = do
                    let q = head elms
                    
                    let res = TEM.contains q tem_f
                    let !res_f = force res

                    when ((tail elms) /= []) (query_loop (tail elms))
                
                query_loop query_elements_f

                -- TODO: is this better? Or is there allocation issues?
                -- let res = map (\elm -> TEM.contains elm tem_f) query_elements_f
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
update_insert_total_runtime_test (tem_empty, tem_insert, _) (per_empty, per_insert, _) = do
    -- TODO: scale seed and repeats automaically
    let size_start = 48269 -- 10000
    let size_incr_mul = 1.3 :: Float
    let size_end = 1000000

    let seed_start = 0
    let seed_end = 15 -- 30

    let repeats = 2  -- 10

    putStrLn "seed,n,tem,per"

    let size_loop size = do
        let seed_loop seed = do
            let repeat_loop itr = do
                let pureGen = mkStdGen seed
                let !random_permutation = random_shuffle size pureGen

                start_tem <- liftIO getCurrentTime
                let tem = foldl (flip tem_insert) tem_empty random_permutation
                let !tem_f = force tem
                end_tem <- liftIO getCurrentTime
                let elapsedTime_tem = realToFrac $ end_tem `diffUTCTime` start_tem

                start_per <- liftIO getCurrentTime
                let per = foldl (flip per_insert) per_empty random_permutation
                let !per_f = force per
                end_per <- liftIO getCurrentTime
                let elapsedTime_per = realToFrac $ end_per `diffUTCTime` start_per

                putStrLn (show seed ++ "," ++ show size ++ "," ++ show elapsedTime_tem ++ "," ++ show elapsedTime_per)
                hFlush stdout

                when (itr + 1 < repeats) (repeat_loop (itr + 1))

            repeat_loop 0

            when (seed < seed_end - 1) (seed_loop (seed + 1))

        seed_loop seed_start

        when (size < size_end) (size_loop (ceiling ((fromIntegral size) * size_incr_mul)))

    size_loop size_start

update_insert_range_total_runtime_test (tem_empty, tem_insert, _) (per_empty, per_insert, _) = do
    let size_start = 1000
    let size_incr_mul = 1.3 :: Float
    let size_end = 10000000

    let repeats = 20

    putStrLn "n,tem,per"

    let size_loop size = do
        let repeat_loop itr = do
            let !elements = [1 :: Int .. size]

            start_tem <- liftIO getCurrentTime
            let tem = foldl (flip tem_insert) tem_empty elements
            let !tem_f = force tem
            end_tem <- liftIO getCurrentTime
            let elapsedTime_tem = realToFrac $ end_tem `diffUTCTime` start_tem

            start_per <- liftIO getCurrentTime
            let per = foldl (flip per_insert) per_empty elements
            let !per_f = force per
            end_per <- liftIO getCurrentTime
            let elapsedTime_per = realToFrac $ end_per `diffUTCTime` start_per

            putStrLn (show size ++ "," ++ show elapsedTime_tem ++ "," ++ show elapsedTime_per)
            hFlush stdout

            when (itr + 1 < repeats) (repeat_loop (itr + 1))

        repeat_loop 0

        when (size < size_end) (size_loop (ceiling ((fromIntegral size) * size_incr_mul)))

    size_loop size_start

update_insert_and_delete_total_runtime_test (tem_empty, tem_insert, tem_delete) (per_empty, per_insert, per_delete) = do
    -- TODO: scale seed and repeats automaically
    let size_start = 10000
    let size_incr_mul = 1.3 :: Float
    let size_end = 1000000

    let seed_start = 0
    -- let seed_end = 30

    -- let repeats = 10

    putStrLn "seed,n,tem,per"

    let size_loop size = do
        let seed_end = if size < 25000 then 30 else 15
        let repeats = if size < 25000 then 10 else 2

        let seed_loop seed = do
            let repeat_loop itr = do
                let pureGen = mkStdGen seed
                let !random_insert_permutation = random_shuffle size pureGen

                let pureGen = mkStdGen (-seed)
                let !random_delete_permutation = random_shuffle size pureGen

                start_tem <- liftIO getCurrentTime
                let tem_base = foldl (flip tem_insert) tem_empty random_insert_permutation
                let tem = foldl (flip tem_delete) tem_base random_delete_permutation
                let !tem_f = force tem
                end_tem <- liftIO getCurrentTime
                let elapsedTime_tem = realToFrac $ end_tem `diffUTCTime` start_tem

                start_per <- liftIO getCurrentTime
                let per_base = foldl (flip per_insert) per_empty random_insert_permutation
                let per = foldl (flip per_delete) per_base random_delete_permutation
                let !per_f = force per
                end_per <- liftIO getCurrentTime
                let elapsedTime_per = realToFrac $ end_per `diffUTCTime` start_per

                putStrLn (show seed ++ "," ++ show size ++ "," ++ show elapsedTime_tem ++ "," ++ show elapsedTime_per)
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

            when (seed < seed_end - 1) (seed_loop (seed + 1))

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



main = do
    -- small_temporal_tree_build TEM.get_func TEM.contains
    -- small_temporal_tree_build RB.get_func RB.member
    -- small_persistent_tree_build PER_M.get_func
    -- small_persistent_tree_build PER.get_func
    -- small_persistent_rotate
    -- small_temporal_list_build
    
    -- correctness_test TEM.get_func PER.get_func
    -- correctness_test RB.get_func RB_per.get_func
    -- delete_persistent_compare
    -- random_access_list_correctness

    -- temporal_tree_node_size_test TEM.get_func
    -- sanity_size_test

    -- size_compare_test (build_binary_tree_without_duplicates TEM.get_func PER.get_func)
    -- size_compare_test (build_and_destroy_binary_tree_without_duplicates TEM.get_func PER.get_func)
    -- size_worst_case_compare_test TEM.get_func PER.get_func
    -- size_worst_case_test PER.get_func

    -- sanity_runtime_check
    -- update_insert_total_runtime_test TEM.get_func PER.get_func
    -- update_insert_and_delete_total_runtime_test TEM.get_func PER.get_func
    -- dag_build_insert_only_speed_test PER.get_func  -- TODO: this one!
    -- dag_build_insert_delete_speed_test PER.get_func
    -- dag_build_worst_case_delete_speed_test PER.get_func

    update_insert_range_total_runtime_test RB.get_func RB_per.get_func

    -- let (tem, per) = build_binary_tree_without_duplicates TEM.get_func PER.get_func 10 1
    -- let tree_10 : tem_rest = tem
    -- putStrLn (pretty_tree tree_10)
