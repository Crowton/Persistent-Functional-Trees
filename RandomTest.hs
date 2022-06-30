{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module RandomTest where

import System.Random
import System.Random.Shuffle

import DataRecords
import RandomAccessListEphemeral as RALEph
import RandomAccessListPersistent as RALPer
import DAGconstruction

import Data.List


rolls :: RandomGen b => Int -> Int -> b -> [Int]
rolls n b = take n . unfoldr (Just . uniformR (0, b))

randomShuffle :: RandomGen b => Int -> b -> [Int]
randomShuffle n = shuffle' [1 .. n] n


applyBinaryTreeUpdates :: (t -> Tree s -> Tree s) -> (t -> PartialTree s -> PartialTree s) -> ([Tree s], PartialTree s) -> [t] -> ([Tree s], PartialTree s)
applyBinaryTreeUpdates ephUpdate perUpdate =
    foldl (\(ephH : ephT, per) element ->
            let nextEph = ephUpdate element ephH in
            let nextPer = perUpdate element per in
            (nextEph : ephH : ephT, nextPer)
    )

buildBinaryTree :: EPH_BST t s -> PER_BST t s -> [t] -> ([Tree s], PartialTree s)
buildBinaryTree (ephEmpty, ephInsert, _) (perEmpty, perInsert, _) =
    applyBinaryTreeUpdates ephInsert perInsert ([ephEmpty], perEmpty)

debuildBinaryTree :: EPH_BST t s -> PER_BST t s -> ([Tree s], PartialTree s) -> [t] -> ([Tree s], PartialTree s)
debuildBinaryTree (_, _, ephDelete) (_, _, perDelete) =
    applyBinaryTreeUpdates ephDelete perDelete


buildBinaryTreeWithDuplicates :: EPH_BST Int s -> PER_BST Int s -> Int -> Int -> ([Tree s], PartialTree s)
buildBinaryTreeWithDuplicates eph per num seed =
    let pureGen = mkStdGen seed in
    let randomElements = rolls num (2 * num) pureGen in
    buildBinaryTree eph per randomElements

buildBinaryTreeWithoutDuplicates :: EPH_BST Int s -> PER_BST Int s -> Int -> Int -> ([Tree s], PartialTree s)
buildBinaryTreeWithoutDuplicates eph per num seed =
    let pureGen = mkStdGen seed in
    let randomPermutation = randomShuffle num pureGen in
    buildBinaryTree eph per randomPermutation

destroyBinaryTreeWithoutDuplicates :: EPH_BST Int s -> PER_BST Int s -> Int -> Int -> ([Tree s], PartialTree s) -> ([Tree s], PartialTree s)
destroyBinaryTreeWithoutDuplicates eph per num seed initialTrees =
    let pureGen = mkStdGen seed in
    let randomPermutation = randomShuffle num pureGen in
    debuildBinaryTree eph per initialTrees randomPermutation


buildAndDestroyBinaryTreeWithoutDuplicates :: EPH_BST Int s -> PER_BST Int s -> Int -> Int -> ([Tree s], PartialTree s)
buildAndDestroyBinaryTreeWithoutDuplicates eph per num seed =
    destroyBinaryTreeWithoutDuplicates eph per num (-seed) (buildBinaryTreeWithoutDuplicates eph per num seed)


getHighOutDegreePaths :: Int -> ([Int], [Int])
getHighOutDegreePaths num =
    ( (reverse [num + 2, num + 4 .. 3 * num]) ++ [1] ++ [num + 3, num + 5 .. 3 * num + 1] ++ [0]
    , reverse [2 .. num + 1]
    )


buildBinaryPersistentTreeHighOutDegree :: PER_BST Int s -> Int -> PartialTree s
buildBinaryPersistentTreeHighOutDegree (perEmpty, perInsert, perDelete) num =
    let (firstPath, secondPath) = getHighOutDegreePaths num in

    -- Build initial tree, using insertion
    let persistentBase =
            foldl (\per element ->
                    perInsert element per
            ) perEmpty (firstPath ++ secondPath)
    in

    -- Make deletion of the second path
    let persistentTree =
            foldl (\per element ->
                    perDelete element per
            ) persistentBase secondPath
    in

    persistentTree


binaryTreeTestInsert :: Eq s => EPH_BST Int s -> PER_BST Int s -> Int -> Bool
binaryTreeTestInsert eph per num =
    let (ephemeralList, persistentTree) = buildBinaryTreeWithDuplicates eph per num 42 in
    let buildPersistentTree = build persistentTree in

    all (\(testTime, ephemeralTree) ->
            ephemeralTree == buildPersistentTree testTime
    ) (zip [0 .. num] (reverse ephemeralList))


binaryTreeTestDelete :: Show s => Eq s => EPH_BST Int s -> PER_BST Int s -> Int -> Bool
binaryTreeTestDelete (ephEmpty, ephInsert, ephDelete) (perEmpty, perInsert, perDelete) num =
    let insertSeed = 42 in
    let deleteSeed = 142 in

    -- Generate elements
    let insertPureGen = mkStdGen insertSeed in
    let initialRandomElements = rolls num (2 * num) insertPureGen in

    -- Build random initial tree, using insertion
    let (ephemeralBase, persistentBase) =
            foldl (\(eph, per) element ->
                    let nextEph = ephInsert element eph in
                    let nextPer = perInsert element per in
                    (nextEph, nextPer)
            ) (ephEmpty, perEmpty) initialRandomElements
    in

    -- Generate elements
    let deletePureGen = mkStdGen deleteSeed in
    let randomElements = rolls num (2 * num) deletePureGen in

    -- Make deletion on tree
    let (ephemeralList, persistentTree) =
            foldl (\(ephH : ephT, per) element ->
                    let nextEph = ephDelete element ephH in
                    let nextPer = perDelete element per in
                    (nextEph : ephH : ephT, nextPer)
            ) ([ephemeralBase], persistentBase) randomElements
    in

    -- Build tree
    let buildPersistentTree = build persistentTree in

    -- Fetch time before the first deletion
    let checkTimeFrom = time persistentBase - 1 in

    -- Check equality
    all (\(testTime, ephemeralTree) ->
            ephemeralTree == buildPersistentTree testTime
    ) (zip [checkTimeFrom .. checkTimeFrom + num] (reverse ephemeralList))


-- Tree contains long left path, one path to the right, and another left path
-- By deleting the element to the right repeatly, the parent gets high out degree,
-- which the dag construction then needs to make smaller
-- To make it worse, each node have a second other child, which is never touched, to force more splits
binaryTreeHighTimeOutDegreeNode :: Eq s => EPH_BST Int s -> PER_BST Int s -> (PartialTree s -> Int -> Tree s) -> Int -> Bool
binaryTreeHighTimeOutDegreeNode (ephEmpty, ephInsert, ephDelete) (perEmpty, perInsert, perDelete) builder num =
    let (firstPath, secondPath) = getHighOutDegreePaths num in

    -- Build initial tree, using insertion
    let (ephemeralBase, persistentBase) =
            foldl (\(eph, per) element ->
                    let nextEph = ephInsert element eph in
                    let nextPer = perInsert element per in
                    (nextEph, nextPer)
            ) (ephEmpty, perEmpty) (firstPath ++ secondPath)
    in

    -- Make deletion of the second path
    let (ephemeralList, persistentTree) =
            foldl (\(ephH : ephT, per) element ->
                    let nextEph = ephDelete element ephH in
                    let nextPer = perDelete element per in
                    (nextEph : ephH : ephT, nextPer)
            ) ([ephemeralBase], persistentBase) secondPath
    in

    -- Build tree
    -- let buildPersistentTree = build persistentTree in
    -- let buildPersistentTree = buildNodeSplit persistentTree in
    let buildPersistentTree = builder persistentTree in

    -- Fetch time before the first deletion
    let checkTimeFrom = time persistentBase - 1 in

    -- Check equality
    all (\(testTime, ephemeralTree) ->
            ephemeralTree == buildPersistentTree testTime
    ) (zip [checkTimeFrom .. checkTimeFrom + num] (reverse ephemeralList))




randomAccessListCons :: Int -> Int -> Bool
randomAccessListCons num insSeed =
    let insPureGen = mkStdGen insSeed in
    let insElements = randomShuffle num insPureGen in

    let (insEphList, insPer) =
            foldl (\(ephH : ephT, per) element ->
                    let nextEph = RALEph.cons element ephH in
                    let nextPer = RALPer.cons element per in
                    (nextEph : ephH : ephT, nextPer)
            ) ([RALEph.empty], RALPer.empty) insElements
    in

    let buildPersistent = build insPer in

    all (\(testTime, ephemeralTree) ->
            ephemeralTree == buildPersistent testTime
    ) (zip [0 .. num] (reverse insEphList))


randomAccessListUpdateUniform :: Int -> Int -> Int -> Int -> Bool
randomAccessListUpdateUniform num insSeed updIndexSeed updElemSeed =
    let insPureGen = mkStdGen insSeed in
    let insElements = randomShuffle num insPureGen in

    let (insEph, insPer) =
            foldl (\(eph, per) element ->
                    let nextEph = RALEph.cons element eph in
                    let nextPer = RALPer.cons element per in
                    (nextEph, nextPer)
            ) (RALEph.empty, RALPer.empty) insElements
    in
    
    let updIndexPureGen = mkStdGen updIndexSeed in
    let updIndexes = rolls num (num - 1) updIndexPureGen in
    
    let updElemPureGen = mkStdGen updElemSeed in
    let updElements = rolls num (4 * num) updElemPureGen in

    let (updEphList, updPer) =
            foldl (\(ephH : ephT, per) (index, element) ->
                    let nextEph = RALEph.update (index, element) ephH in
                    let nextPer = RALPer.update (index, element) per in
                    (nextEph : ephH : ephT, nextPer)
            ) ([insEph], insPer) (zip updIndexes updElements)
    in

    let buildPersistent = build updPer in
    
    let checkTimeFrom = time insPer - 1 in

    all (\(testTime, ephemeralTree) ->
            ephemeralTree == buildPersistent testTime
    ) (zip [checkTimeFrom .. checkTimeFrom + num] (reverse updEphList))


randomAccessListTail :: Int -> Int -> Bool
randomAccessListTail num insSeed =
    let insPureGen = mkStdGen insSeed in
    let insElements = randomShuffle num insPureGen in
    
    let (insEph, insPer) =
            foldl (\(eph, per) element ->
                    let nextEph = RALEph.cons element eph in
                    let nextPer = RALPer.cons element per in
                    (nextEph, nextPer)
            ) (RALEph.empty, RALPer.empty) insElements
    in

    let (tailEphList, tailPer) =
            foldl (\(ephH : ephT, per) _ ->
                    let nextEph = RALEph.tail ephH in
                    let nextPer = RALPer.tail per in
                    (nextEph : ephH : ephT, nextPer)
            ) ([insEph], insPer) [0 .. num - 1]
    in

    let buildPersistent = build tailPer in
    
    let checkTimeFrom = time insPer - 1 in

    all (\(testTime, ephemeralTree) ->
            ephemeralTree == buildPersistent testTime
    ) (zip [checkTimeFrom .. checkTimeFrom + num - 1] (reverse tailEphList))


randomAccessListUpdateFinalElement :: Int -> Int -> Int -> Bool
randomAccessListUpdateFinalElement num insSeed updElemSeed =
    let insPureGen = mkStdGen insSeed in
    let insElements = randomShuffle num insPureGen in

    let (insEph, insPer) =
            foldl (\(eph, per) element ->
                    let nextEph = RALEph.cons element eph in
                    let nextPer = RALPer.cons element per in
                    (nextEph, nextPer)
            ) (RALEph.empty, RALPer.empty) insElements
    in
    
    let updElemPureGen = mkStdGen updElemSeed in
    let updElements = rolls num (4 * num) updElemPureGen in

    let (updEphList, updPer) =
            foldl (\(ephH : ephT, per) element ->
                    let nextEph = RALEph.update (num - 1, element) ephH in
                    let nextPer = RALPer.update (num - 1, element) per in
                    (nextEph : ephH : ephT, nextPer)
            ) ([insEph], insPer) updElements
    in

    let buildPersistent = build updPer in
    
    let checkTimeFrom = time insPer - 1 in

    all (\(testTime, ephemeralTree) ->
            ephemeralTree == buildPersistent testTime
    ) (zip [checkTimeFrom .. checkTimeFrom + num] (reverse updEphList))
