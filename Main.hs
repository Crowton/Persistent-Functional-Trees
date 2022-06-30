{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import Data.List as L

import Data.Function
import System.IO

import DataRecords

import BinaryTreeEphemeral as EPH
import BinaryTreePersistentMock as PERmock
import BinaryTreePersistent as PER

import RBTreeEphemeral as RB
import RBTreePersistent as RBPer

import qualified RandomAccessListEphemeral as RAL
import qualified RandomAccessListPersistent as RALPer

import PersistentUpdate
import DAGconstruction
import TreeConstructor

import qualified Data.Map.Strict as MB

import RandomTest

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

smallEphemeralTreeBuild (ephEmpty, ephInsert, ephDelete) ephContains = do
    let tree =
            ephEmpty
            & ephInsert 3
            & ephInsert 1
            & ephInsert 2
            & ephInsert 4
            & ephDelete 3

    putStrLn (prettyTree tree)
    putStrLn ("Contains 1: " ++ (show (ephContains 1 tree)) ++ "\n")

    let tree2 =
            ephEmpty
            & ephInsert 1
            & ephInsert 2
            & ephInsert 3
            & ephInsert 4
            & ephInsert 5
            & ephInsert 6
            & ephInsert 7
            & ephInsert 8
            & ephInsert 9
            & ephDelete 4

    putStrLn (prettyTree tree2)

smallPersistentTreeBuild (perEmpty, perInsert, perDelete) = do
    let persistentTree =
            perEmpty
            & perInsert 3
            & perInsert 3
            & perInsert 2
            & perInsert 1
            & perInsert 4
            & perDelete 2
            & perDelete 4
            & perDelete 5

    let buildTree = build persistentTree

    -- putStrLn ("TimeTree\n" ++ prettyTimeTree (fst (head (rootList persistentTree))) (currentTree persistentTree) ++ "\n")

    putStrLn ("Time 0:\n" ++ prettyTree (buildTree 0) ++ "\n")
    putStrLn ("Time 1:\n" ++ prettyTree (buildTree 1) ++ "\n")
    putStrLn ("Time 2:\n" ++ prettyTree (buildTree 2) ++ "\n")
    putStrLn ("Time 3:\n" ++ prettyTree (buildTree 3) ++ "\n")
    putStrLn ("Time 4:\n" ++ prettyTree (buildTree 4) ++ "\n")
    putStrLn ("Time 5:\n" ++ prettyTree (buildTree 5) ++ "\n")
    putStrLn ("Time 6:\n" ++ prettyTree (buildTree 6) ++ "\n")
    putStrLn ("Time 7:\n" ++ prettyTree (buildTree 7) ++ "\n")
    putStrLn ("Time 8:\n" ++ prettyTree (buildTree 8) ++ "\n")
    putStrLn ("Time 9:\n" ++ prettyTree (buildTree 9) ++ "\n")

smallPersistentRotate = do
    let perTree =
            PER.empty
            & PER.insert 6
            & PER.insert 2
            & PER.insert 7
            & PER.insert 1
            & PER.insert 4
            & PER.insert 3
            & PER.insert 5
            & update (\_ -> PER.rotateRightLeft) Nothing

    let tree = build perTree

    putStrLn ("Before rotation:\n" ++ prettyTree (tree 7) ++ "\n")
    putStrLn ("After rotation:\n" ++ prettyTree (tree 8) ++ "\n")

smallEphemeralListBuild = do
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
    
    putStrLn ((prettyTree list) ++ "\n")

    putStrLn ("Head: " ++ show (RAL.head list) ++ "\n")
    putStrLn ("Lookup 5: " ++ show (RAL.lookup 5 list) ++ "\n")

    let list2 =
            list
            & RAL.update (7, 11)
    
    putStrLn ((prettyTree list2) ++ "\n")

    let list3 =
            list2
            & RAL.tail
            & RAL.tail
            & RAL.tail
    
    putStrLn ((prettyTree list3) ++ "\n")


-- Tests for correctness --

correctnessTest ephBuild perBuild = do
    putStrLn "Running tests"
    hFlush stdout

    let testRun name test =
            do { putStr name
               ; hFlush stdout

               ; if test
                    then putStrLn "Success"
                    else error "Test failed!"
               }
        
    -- Insertion
    testRun
        "Insertion test ............ "
        (binaryTreeTestInsert ephBuild perBuild 1000)

    -- Deletion
    testRun
        "Deletion test ............. "
        (binaryTreeTestDelete ephBuild perBuild 1000)

    -- Deletion, which creates notes with high out degree
    let size = 1000
    testRun
        "Node non splitting test ... "
        (binaryTreeHighTimeOutDegreeNode ephBuild perBuild buildNonSplit size)
    
    testRun
        "Node splitting test ....... "
        (binaryTreeHighTimeOutDegreeNode ephBuild perBuild build size)

deletePersistentCompare = do
    let buildElm = [2, 5, 8, 3, 9, 0, 1]
    let delElm   = [0, 5, 3, 8, 1, 2, 9]

    let perMock = foldl (\t e -> PERmock.insert e t) PERmock.constructEmptyTree buildElm
    let perReal = foldl (\t e -> PER.insert e t) PER.empty buildElm

    putStrLn ("Initial tree:\n" ++ (prettyTimeTree 1 (currentTree perMock)) ++ "\n")

    let eqPer PartialTree {edgeFreezer=edgeFreezer1
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

            let newMock = PERmock.delete elm mock
            let newReal = PER.delete elm real

            putStrLn ("After deleting " ++ show elm ++ ":")
            if eqPer newMock newReal
                then putStrLn ("All good! The tree is now:\n" ++ (prettyTimeTree 1 (currentTree newMock)) ++ "\n")
                else putStrLn ("Mock:\n" ++ show newMock ++ "\n\n"
                            ++ "Real:\n" ++ show newReal ++ "\n")

            when ((tail elements) /= []) (loop (tail elements) newMock newReal)

    loop delElm perMock perReal

randomAccessListCorrectness = do
    putStrLn "Running tests"
    hFlush stdout

    let testRun name test =
            do { putStr name
               ; hFlush stdout

               ; if test
                    then putStrLn "Success"
                    else error "Test failed!"
               }
        
    -- Cons
    testRun
        "Cons test .......... "
        (randomAccessListCons 1000 42)

    -- Update
    testRun
        "Update test ........ "
        (randomAccessListUpdateUniform 1000 42 43 44)

    -- Tail
    testRun
        "Tail test .......... "
        (randomAccessListTail 5 42)

    -- Update in end
    testRun
        "Update last tast ... "
        (randomAccessListUpdateFinalElement 1000 42 43)


-- Checks for size of objects -- 

ephemeralTreeNodeSizeTest (ephEmpty, ephInsert, _) = do
    let tree0 = ephEmpty :: Tree Int
    let tree1
            = tree0
            & ephInsert 1

    size0 <- recursiveSizeNF tree0
    size1 <- recursiveSizeNF tree1

    putStrLn ("Size of leaf: " ++ show size0)
    putStrLn ("Delta size of single insert (0 -> 1): " ++ show (size1 - size0))

    let tree2
            = tree1
            & ephInsert 2

    size2 <- recursiveSizeNF tree2

    putStrLn ("Delta size of single insert (1 -> 2): " ++ show (size2 - size1))

    let tree7
            = tree0
            & ephInsert 4
            & ephInsert 2
            & ephInsert 6
            & ephInsert 1
            & ephInsert 3
            & ephInsert 5
            & ephInsert 7
    let tree8
            = tree7
            & ephInsert 8

    size7 <- recursiveSizeNF tree7
    size8 <- recursiveSizeNF tree8

    putStrLn ("Delta size of single insert (7 -> 8): " ++ show (size8 - size7))

    let tree9
            = tree8
            & ephInsert 0

    size9 <- recursiveSizeNF tree9

    putStrLn ("Delta size of single insert (8 -> 9): " ++ show (size9 - size8))

    putStrLn ("Size of tree 0: " ++ show size0)
    putStrLn ("Size of tree 1: " ++ show size1)
    putStrLn ("Size of tree 2: " ++ show size2)
    putStrLn ("Size of tree 7: " ++ show size7)
    putStrLn ("Size of tree 8: " ++ show size8)
    putStrLn ("Size of tree 9: " ++ show size9)

sanitySizeTest = do
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

    -- intSize <- recursiveSizeNF (1 :: Int)
    -- putStrLn (show intSize)

    -- intSize <- recursiveSizeNF (2147483647 :: Int)
    -- putStrLn (show intSize)

    -- intSize <- recursiveSizeNF (9223372036854775807 :: Int)
    -- putStrLn (show intSize)

    -- intSize <- recursiveSizeNF (9223372036854775807 :: Int)
    -- putStrLn (show intSize)


-- Tests for size of build dag --

sizeCompareTest builder = do
    let sizeStart = 10
    let sizeIncrMul = 1.3 :: Float
    let sizeEnd = 1000

    let seedStart = 0
    let seedEnd = 30

    putStrLn "seed,n,eph,per"

    let sizeLoop size = do
        let seedLoop seed = do
            let (eph, per) = builder size seed
            let (perRootList, _) = buildRootList per

            ephSize <- recursiveSizeNF eph
            perSize <- recursiveSizeNF perRootList

            putStrLn (show seed ++ "," ++ show size ++ "," ++ show ephSize ++ "," ++ show perSize)
            hFlush stdout

            when (seed + 1 < seedEnd) (seedLoop (seed + 1))

        seedLoop seedStart

        when (size < sizeEnd) (sizeLoop (ceiling ((fromIntegral size) * sizeIncrMul)))

    sizeLoop sizeStart

sizeWorstCaseCompareTest (ephEmpty, ephInsert, ephDelete) (perEmpty, perInsert, perDelete) = do
    let sizeStart = 10
    let sizeIncrMul = 1.3 :: Float
    let sizeEnd = 10000

    putStrLn "n,eph,per,splits"

    let sizeLoop size = do
        -- TRUE n = 3 * n
        let (ephBase, perBase) = foldl (\(ephH : ephT, per) element ->
                                            let nextEph = ephInsert element ephH in
                                            let nextPer = perInsert element per in
                                            (nextEph : ephH : ephT, nextPer)
                                   ) ([ephEmpty], perEmpty) [1 :: Int .. size]
        
        let (ephFinal, perFinal) = foldl (\(ephH : ephT, per) _ ->
                                              let nextEph = ephInsert (size + 1) ephH in
                                              let nextNextEph = ephDelete (size + 1) nextEph in
                                              let nextPer = perDelete (size + 1) (perInsert (size + 1) per) in
                                              (nextNextEph : nextEph : ephH : ephT, nextPer)
                                     ) (ephBase, perBase) [1 :: Int .. size]

        let (perRootList, splits) = buildRootList perFinal

        ephSize <- recursiveSizeNF ephFinal
        perSize <- recursiveSizeNF perRootList

        putStrLn (show size ++ "," ++ show ephSize ++ "," ++ show perSize ++ "," ++ show splits)
        hFlush stdout

        when (size < sizeEnd) (sizeLoop (ceiling ((fromIntegral size) * sizeIncrMul)))

    sizeLoop sizeStart


sizeWorstCaseTest (perEmpty, perInsert, perDelete) = do
    let sizeStart = 10
    let sizeIncrMul = 1.3 :: Float
    let sizeEnd = 10000000

    putStrLn "n,per,splits"

    let sizeLoop size = do
        -- TRUE n = 3 * n
        let perBase = foldl (flip perInsert) perEmpty [1 :: Int .. size]
        let per = foldl (\p _ -> perDelete (size + 1) (perInsert (size + 1) p)) perBase [1 .. size]

        let (perRootList, splits) = buildRootList per

        perSize <- recursiveSizeNF perRootList

        putStrLn (show size ++ "," ++ show perSize ++ "," ++ show splits)
        hFlush stdout

        when (size < sizeEnd) (sizeLoop (ceiling ((fromIntegral size) * sizeIncrMul)))

    sizeLoop sizeStart


sizeWorstCaseRangeTest (perEmpty, perInsert, perDelete) = do
    let sizeStart = 1
    let sizeEnd = 10000

    putStrLn "n,per,splits"

    let sizeLoop size = do
        -- TRUE n = 3 * n
        let perBase = foldl (flip perInsert) perEmpty [1 :: Int .. size]
        let per = foldl (\p _ -> perDelete (size + 1) (perInsert (size + 1) p)) perBase [1 .. size]

        let (perRootList, splits) = buildRootList per

        perSize <- recursiveSizeNF perRootList

        putStrLn (show size ++ "," ++ show perSize ++ "," ++ show splits)
        hFlush stdout

        when (size + 1 < sizeEnd) (sizeLoop (size + 1))

    sizeLoop sizeStart


-- Tests for run time --

sanityRuntimeCheck = do
    let sizeStart = 1000
    let sizeIncrMul = 1.3 :: Float
    let sizeEnd = 100000000

    let seedStart = 0
    let seedEnd = 30

    let repeats = 10

    putStrLn "seed,n,time"

    let sizeLoop size = do
        let seedLoop seed = do
            let ephBuilder :: Ord e => [e] -> Tree e -> Tree e
                ephBuilder elements tree =
                    case elements of
                        [] -> tree
                        _  -> let (left, mid : right) = splitAt ((length elements) `div` 2) elements in
                              ephBuilder right (ephBuilder left (EPH.insert mid tree))
            
            let eph = ephBuilder [1 .. size] Leaf
            let !ephF = force eph

            let pureGen = mkStdGen seed
            let queryElements = randomShuffle size pureGen
            let !queryElementsF = force queryElements

            let repeatLoop itr = do
                start <- liftIO getCurrentTime

                let queryLoop elms = do
                    let q = head elms
                    
                    let res = EPH.contains q ephF
                    let !resF = force res

                    when ((tail elms) /= []) (queryLoop (tail elms))
                
                queryLoop queryElementsF

                end <- liftIO getCurrentTime

                let elapsedTime = realToFrac $ end `diffUTCTime` start

                putStrLn (show seed ++ "," ++ show size ++ "," ++ show elapsedTime)
                hFlush stdout

                when (itr + 1 < repeats) (repeatLoop (itr + 1))

            repeatLoop 0

            when (seed < seedEnd - 1) (seedLoop (seed + 1))

        seedLoop seedStart

        when (size < sizeEnd) (sizeLoop (ceiling ((fromIntegral size) * sizeIncrMul)))

    sizeLoop sizeStart


smallDagBuild (perEmpty, perInsert, perDelete) = do
    -- let compare' a b = (trace (show (a, b))) $ compare a b
    -- let l = sortBy compare' [9, 8, 7, 6, 5, 1, 2, 3, 4]
    -- putStrLn (show l)

    let persistentTree =
            perEmpty
            & perInsert (2::Int)
            & perInsert 3
            & perInsert 1
            & perInsert 4
            & perDelete 4 & perInsert 4
            & perDelete 4 & perInsert 4
            & perDelete 4 & perInsert 4
            & perDelete 4 & perInsert 4

    let !buildTreeThing = force (buildRootList persistentTree)

    putStrLn (show (currentTree persistentTree))


updateInsertTotalRuntimeTest (ephEmpty, ephInsert, _) (perEmpty, perInsert, _) = do
    let sizeStart = 48269 -- 10000
    let sizeIncrMul = 1.3 :: Float
    let sizeEnd = 1000000

    let seedStart = 0
    let seedEnd = 15 -- 30

    let repeats = 2  -- 10

    putStrLn "seed,n,eph,per"

    let sizeLoop size = do
        let seedLoop seed = do
            let repeatLoop itr = do
                let pureGen = mkStdGen seed
                let !randomPermutation = randomShuffle size pureGen

                startEph <- liftIO getCurrentTime
                let eph = foldl (flip ephInsert) ephEmpty randomPermutation
                let !ephF = force eph
                endEph <- liftIO getCurrentTime
                let elapsedTimeEph = realToFrac $ endEph `diffUTCTime` startEph

                startPer <- liftIO getCurrentTime
                let per = foldl (flip perInsert) perEmpty randomPermutation
                let !perF = force per
                endPer <- liftIO getCurrentTime
                let elapsedTimePer = realToFrac $ endPer `diffUTCTime` startPer

                putStrLn (show seed ++ "," ++ show size ++ "," ++ show elapsedTimeEph ++ "," ++ show elapsedTimePer)
                hFlush stdout

                when (itr + 1 < repeats) (repeatLoop (itr + 1))

            repeatLoop 0

            when (seed < seedEnd - 1) (seedLoop (seed + 1))

        seedLoop seedStart

        when (size < sizeEnd) (sizeLoop (ceiling ((fromIntegral size) * sizeIncrMul)))

    sizeLoop sizeStart

updateInsertRangeTotalRuntimeTest (ephEmpty, ephInsert, _) (perEmpty, perInsert, _) = do
    let sizeStart = 1000
    let sizeIncrMul = 1.3 :: Float
    let sizeEnd = 10000000

    let repeats = 20

    putStrLn "n,eph,per"

    let sizeLoop size = do
        let repeatLoop itr = do
            let !elements = [1 :: Int .. size]

            startEph <- liftIO getCurrentTime
            let eph = foldl (flip ephInsert) ephEmpty elements
            let !ephF = force eph
            endEph <- liftIO getCurrentTime
            let elapsedTimeEph = realToFrac $ endEph `diffUTCTime` startEph

            startPer <- liftIO getCurrentTime
            let per = foldl (flip perInsert) perEmpty elements
            let !perF = force per
            endPer <- liftIO getCurrentTime
            let elapsedTimePer = realToFrac $ endPer `diffUTCTime` startPer

            putStrLn (show size ++ "," ++ show elapsedTimeEph ++ "," ++ show elapsedTimePer)
            hFlush stdout

            when (itr + 1 < repeats) (repeatLoop (itr + 1))

        repeatLoop 0

        when (size < sizeEnd) (sizeLoop (ceiling ((fromIntegral size) * sizeIncrMul)))

    sizeLoop sizeStart

updateInsertAndDeleteTotalRuntimeTest (ephEmpty, ephInsert, ephDelete) (perEmpty, perInsert, perDelete) = do
    let sizeStart = 10000
    let sizeIncrMul = 1.3 :: Float
    let sizeEnd = 1000000

    let seedStart = 0

    putStrLn "seed,n,eph,per"

    let sizeLoop size = do
        let seedEnd = if size < 25000 then 30 else 15
        let repeats = if size < 25000 then 10 else 2

        let seedLoop seed = do
            let repeatLoop itr = do
                let pureGen = mkStdGen seed
                let !randomInsertPermutation = randomShuffle size pureGen

                let pureGen = mkStdGen (-seed)
                let !randomDeletePermutation = randomShuffle size pureGen

                startEph <- liftIO getCurrentTime
                let ephBase = foldl (flip ephInsert) ephEmpty randomInsertPermutation
                let eph = foldl (flip ephDelete) ephBase randomDeletePermutation
                let !ephF = force eph
                endEph <- liftIO getCurrentTime
                let elapsedTimeEph = realToFrac $ endEph `diffUTCTime` startEph

                startPer <- liftIO getCurrentTime
                let perBase = foldl (flip perInsert) perEmpty randomInsertPermutation
                let per = foldl (flip perDelete) perBase randomDeletePermutation
                let !perF = force per
                endPer <- liftIO getCurrentTime
                let elapsedTimePer = realToFrac $ endPer `diffUTCTime` startPer

                putStrLn (show seed ++ "," ++ show size ++ "," ++ show elapsedTimeEph ++ "," ++ show elapsedTimePer)
                hFlush stdout

                when (itr + 1 < repeats) (repeatLoop (itr + 1))

            repeatLoop 0

            when (seed < seedEnd - 1) (seedLoop (seed + 1))

        seedLoop seedStart

        when (size < sizeEnd) (sizeLoop (ceiling ((fromIntegral size) * sizeIncrMul)))

    sizeLoop sizeStart

dagBuildInsertOnlySpeedTest (perEmpty, perInsert, perDelete) = do
    let sizeStart = 500
    let sizeIncrMul = 1.3 :: Float
    let sizeEnd = 20000000

    let seedStart = 0

    putStrLn "seed,n,time"

    let sizeLoop size = do
        let seedEnd = if size < 10000 then 30 else 15
        let repeats = if size < 10000 then 10 else 2

        let seedLoop seed = do
            let pureGen = mkStdGen seed
            let randomPermutation = randomShuffle size pureGen

            let per = foldl (flip perInsert) perEmpty randomPermutation
            let !perF = force per

            let repeatLoop itr = do
                -- Record building step
                start <- liftIO getCurrentTime
                let (rootList, splits) = buildRootList perF
                let !rootListF = force rootList
                let !splitsF = force splits
                end <- liftIO getCurrentTime

                let elapsedTime = realToFrac $ end `diffUTCTime` start

                putStrLn (show seed ++ "," ++ show size ++ "," ++ show elapsedTime ++ "," ++ show splitsF)
                hFlush stdout

                when (itr + 1 < repeats) (repeatLoop (itr + 1))

            repeatLoop 0

            when (seed < seedEnd - 1) (seedLoop (seed + 1))

        seedLoop seedStart

        when (size < sizeEnd) (sizeLoop (ceiling ((fromIntegral size) * sizeIncrMul)))

    sizeLoop sizeStart

dagBuildInsertDeleteSpeedTest (perEmpty, perInsert, perDelete) = do
    let sizeStart = 500
    let sizeIncrMul = 1.3 :: Float
    let sizeEnd = 20000

    let seedStart = 0

    putStrLn "seed,n,time,splits"

    let sizeLoop size = do
        let seedEnd = if size < 5000 then 30 else 15
        let repeats = if size < 5000 then 10 else 2

        let seedLoop seed = do
            let pureGen = mkStdGen seed
            let randomPermutationInsert = randomShuffle size pureGen

            let pureGen = mkStdGen (-seed)
            let randomPermutationDelete = randomShuffle size pureGen

            let per = foldl (flip perDelete) (foldl (flip perInsert) perEmpty randomPermutationInsert) randomPermutationDelete
            let !perF = force per

            let repeatLoop itr = do
                -- Record building step
                start <- liftIO getCurrentTime
                let (rootList, splits) = buildRootList perF
                let !rootListF = force rootList
                let !splitsF = force splits
                end <- liftIO getCurrentTime

                let elapsedTime = realToFrac $ end `diffUTCTime` start

                -- assert (splitsF <= 2 * size * 3) $ putStrLn "okay"

                putStrLn (show seed ++ "," ++ show size ++ "," ++ show elapsedTime ++ "," ++ show splitsF)
                hFlush stdout

                when (itr + 1 < repeats) (repeatLoop (itr + 1))

            repeatLoop 0

            when (seed + 1 < seedEnd) (seedLoop (seed + 1))

        seedLoop seedStart

        when (size < sizeEnd) (sizeLoop (ceiling ((fromIntegral size) * sizeIncrMul)))

    sizeLoop sizeStart

dagBuildWorstCaseDeleteSpeedTest (perEmpty, perInsert, perDelete) = do
    let sizeStart = 500
    let sizeIncrMul = 1.3 :: Float
    let sizeEnd = 4000

    let repeats = 30

    putStrLn "n,time,splits"

    let sizeLoop size = do
        -- TRUE n = 3 * n
        let perBase = foldl (flip perInsert) perEmpty [1 :: Int .. size]
        let per = foldl (\p _ -> perDelete (size + 1) (perInsert (size + 1) p)) perBase [1 .. size]
        let !perF = force per

        let repeatLoop itr = do
            -- Record building step
            start <- liftIO getCurrentTime
            let (rootList, splits) = buildRootList perF
            let !rootListF = force rootList
            let !splitsF = force splits
            end <- liftIO getCurrentTime

            let elapsedTime = realToFrac $ end `diffUTCTime` start

            putStrLn (show size ++ "," ++ show elapsedTime ++ "," ++ show splitsF)
            hFlush stdout

            when (itr + 1 < repeats) (repeatLoop (itr + 1))

        repeatLoop 0

        when (size < sizeEnd) (sizeLoop (ceiling ((fromIntegral size) * sizeIncrMul)))

    sizeLoop sizeStart


queryOnlyInsertsFixedSizeSumElementsRuntimeTest (ephEmpty, ephInsert, _) (perEmpty, perInsert, _) = do
    let timeStart = 1000
    let timeIncrMul = 1.3 :: Float
    
    let size = 200000
    
    let seedStart = 0
    let seedEnd = 40

    let repeats = 20

    putStrLn "seed,version,eph,per"

    let seedLoop seed = do
        let pureGen = mkStdGen seed
        let !randomPermutation = randomShuffle size pureGen

        let (_ : ephList, _, times) = foldl 
                (\(ephH : ephT, nextTime, times) (time, elm) ->
                    let newEph = ephInsert elm ephH in
                    if (time == nextTime)
                        then let newNextTime = ceiling ((fromIntegral nextTime) * timeIncrMul) in
                             (newEph : newEph : ephT, newNextTime, time : times)
                        else (newEph : ephT, nextTime, times)
                )
                ([ephEmpty], timeStart, [])
                (zip [1..] randomPermutation)
        
        let !ephListF = force (reverse ephList)
        let !timesF = force (reverse times)

        let per = foldl (flip perInsert) perEmpty randomPermutation
        let (rootNodeList, _) = buildRootList per
        let rootMap = MB.fromDistinctDescList rootNodeList
        let !rootMapF = force rootMap

        let perTree = construct (fieldCount per) rootMap


        let timeLoop times ephs = do
            let !time = force (head times)
            let !eph = force (head ephs)
            
            let repeatLoop itr = do
                startEph <- liftIO getCurrentTime
                let ephSum = EPH.sum eph
                let !ephF = force ephSum
                endEph <- liftIO getCurrentTime
                let elapsedTimeEph = realToFrac $ endEph `diffUTCTime` startEph

                startPer <- liftIO getCurrentTime
                let perSum = EPH.sum (perTree time)
                let !perF = force perSum
                endPer <- liftIO getCurrentTime
                let elapsedTimePer = realToFrac $ endPer `diffUTCTime` startPer

                putStrLn (show seed ++ "," ++ show time ++ "," ++ show elapsedTimeEph ++ "," ++ show elapsedTimePer)
                hFlush stdout

                when (itr + 1 < repeats) (repeatLoop (itr + 1))

            repeatLoop 0

            when (tail times /= []) (timeLoop (tail times) (tail ephs))
        
        timeLoop timesF ephListF

        when (seed + 1 < seedEnd) (seedLoop (seed + 1))

    seedLoop seedStart


queryOnlyInsertsRangeFixedSizeSumElementsRuntimeTest (ephEmpty, ephInsert, _) (perEmpty, perInsert, _) = do
    let timeStart = 1000
    let timeIncrMul = 1.3 :: Float
    
    let size = 100000

    let repeats = 30

    putStrLn "version,eph,per"

    let !elements = [1::Int .. size]

    let (_ : ephList, _, times) = foldl 
            (\(ephH : ephT, nextTime, times) (time, elm) ->
                let newEph = ephInsert elm ephH in
                if (time == nextTime)
                    then let newNextTime = ceiling ((fromIntegral nextTime) * timeIncrMul) in
                            (newEph : newEph : ephT, newNextTime, time : times)
                    else (newEph : ephT, nextTime, times)
            )
            ([ephEmpty], timeStart, [])
            (zip [1..] elements)
    
    let !ephListF = force (reverse ephList)
    let !timesF = force (reverse times)

    let per = foldl (flip perInsert) perEmpty elements
    let (rootNodeList, _) = buildRootList per
    let rootMap = MB.fromDistinctDescList rootNodeList
    let !rootMapF = force rootMap

    let perTree = construct (fieldCount per) rootMap


    let timeLoop times ephs = do
        let !time = force (head times)
        let !eph = force (head ephs)
        
        let repeatLoop itr = do
            startEph <- liftIO getCurrentTime
            let ephSum = RB.sum eph
            let !ephF = force ephSum
            endEph <- liftIO getCurrentTime
            let elapsedTimeEph = realToFrac $ endEph `diffUTCTime` startEph

            startPer <- liftIO getCurrentTime
            let perSum = RB.sum (perTree time)
            let !perF = force perSum
            endPer <- liftIO getCurrentTime
            let elapsedTimePer = realToFrac $ endPer `diffUTCTime` startPer

            putStrLn (show time ++ "," ++ show elapsedTimeEph ++ "," ++ show elapsedTimePer)
            hFlush stdout

            when (itr + 1 < repeats) (repeatLoop (itr + 1))

        repeatLoop 0

        when (tail times /= []) (timeLoop (tail times) (tail ephs))
    
    timeLoop timesF ephListF


queryWorstCaseInsertDeleteFixedSizeContainsLowLeafRuntimeTest (ephEmpty, ephInsert, _) (perEmpty, perInsert, perDelete) = do
    let timeStart = 10
    let timeIncrMul = 1.2 :: Float
    
    let size = 3000

    let timeEnd = 3 * size
    let realTimeStart = timeStart + size
    
    let repeats = 30

    putStrLn "version,eph,per"


    let ephLeaf = foldl (flip ephInsert) ephEmpty [1 :: Int .. size]
    let ephNode = ephInsert (size + 1) ephLeaf

    let perBase = foldl (flip perInsert) perEmpty [1 :: Int .. size]
    let per = foldl (\p _ -> perDelete (size + 1) (perInsert (size + 1) p)) perBase [1 .. size]

    let (rootNodeList, _) = buildRootList per
    let rootMap = MB.fromDistinctDescList rootNodeList
    let !rootMapF = force rootMap

    let perTree = construct (fieldCount per) rootMap
    
    let timeLoop time = do
        let !eph = force (if (EPH.contains (size + 1) (perTree time)) then ephNode else ephLeaf) 

        let repeatLoop itr = do
            startEph <- liftIO getCurrentTime
            let ephRes = EPH.contains (size + 1) eph
            let !ephF = force ephRes
            endEph <- liftIO getCurrentTime
            let elapsedTimeEph = realToFrac $ endEph `diffUTCTime` startEph

            startPer <- liftIO getCurrentTime
            let perRes = EPH.contains (size + 1) (perTree time)
            let !perF = force perRes
            endPer <- liftIO getCurrentTime
            let elapsedTimePer = realToFrac $ endPer `diffUTCTime` startPer

            putStrLn (show time ++ "," ++ show elapsedTimeEph ++ "," ++ show elapsedTimePer)
            hFlush stdout

            when (itr + 1 < repeats) (repeatLoop (itr + 1))

        repeatLoop 0

        let newTime = ceiling ((fromIntegral time) * timeIncrMul)

        when (newTime < timeEnd) (timeLoop newTime)
    
    timeLoop realTimeStart


main = do
    smallEphemeralTreeBuild EPH.getFunc EPH.contains
    smallEphemeralTreeBuild RB.getFunc RB.member
    smallPersistentTreeBuild PERmock.getFunc
    smallPersistentTreeBuild PER.getFunc
    smallPersistentRotate
    smallEphemeralListBuild
    
    correctnessTest EPH.getFunc PER.getFunc
    correctnessTest RB.getFunc RBPer.getFunc
    deletePersistentCompare
    randomAccessListCorrectness

    ephemeralTreeNodeSizeTest EPH.getFunc
    sanitySizeTest

    sizeCompareTest (buildBinaryTreeWithoutDuplicates EPH.getFunc PER.getFunc)
    sizeCompareTest (buildAndDestroyBinaryTreeWithoutDuplicates EPH.getFunc PER.getFunc)
    sizeWorstCaseCompareTest EPH.getFunc PER.getFunc
    sizeWorstCaseTest PER.getFunc
    sizeWorstCaseRangeTest PER.getFunc

    updateInsertTotalRuntimeTest EPH.getFunc PER.getFunc
    updateInsertAndDeleteTotalRuntimeTest EPH.getFunc PER.getFunc

    sanityRuntimeCheck
    smallDagBuild PER.getFunc
    dagBuildInsertOnlySpeedTest PER.getFunc
    dagBuildInsertDeleteSpeedTest PER.getFunc
    dagBuildWorstCaseDeleteSpeedTest PER.getFunc

    queryOnlyInsertsFixedSizeSumElementsRuntimeTest EPH.getFunc PER.getFunc
    queryOnlyInsertsRangeFixedSizeSumElementsRuntimeTest RB.getFunc RBPer.getFunc
    queryWorstCaseInsertDeleteFixedSizeContainsLowLeafRuntimeTest EPH.getFunc PER.getFunc

    updateInsertRangeTotalRuntimeTest RB.getFunc RBPer.getFunc

    let (eph, per) = buildBinaryTreeWithoutDuplicates EPH.getFunc PER.getFunc 10 1
    let tree10 : ephRest = eph
    putStrLn (prettyTree tree10)
