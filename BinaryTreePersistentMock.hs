{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module BinaryTreePersistentMock where

import DataRecords


getFunc :: Ord s => PER_BST s s
getFunc = (constructEmptyTree, insert, delete)


-- Function to construct inital empty tree.
-- There are no frozen edges and id's, the time is 0 and the degree 2.
-- Initially, the empty tree is the root.
constructEmptyTree :: PartialTree s
constructEmptyTree =
    PartialTree {
        edgeFreezer = [],
        idStaticList = [],
        rootList = [(0, -1)],
        idCount = 0,
        fieldCount = 2,
        time = 1,
        currentTree = TimeLeaf
    }


-- Function to insert element into binary tree
insert :: Ord s => s -> PartialTree s -> PartialTree s
insert e partialTree =
    -- Fetch next id and time
    let nextId = idCount partialTree in
    let currentTime = time partialTree in
    
    -- Function to insert tree, and label time on edges
    let innerInsert tree
            = case tree of
                -- Empty node is where the element is to be inserted.
                -- Create new node, with two leaf children, recording the current time.
                TimeLeaf ->
                    ( TimeNode
                        { tElm = e
                        , tId = nextId
                        , tFields = [(currentTime, TimeLeaf), (currentTime, TimeLeaf)]
                        }
                    , True
                    )
                
                -- On a node, the element in the node and the new element must be compared
                TimeNode {tElm=nodeElm, tId=nodeId, tFields=[(leftTime, leftTree), (rightTime, rightTree)]} ->
                    if e == nodeElm
                        -- The elements are the same => element already exists, so return the same tree
                        then ( tree, False )

                        -- Else check which child to recurse into
                        else if e < nodeElm
                                -- Smaller => left child recursion
                                -- If left child was leaf before, then the new element is inserted here. Update time on edge
                                then let (newLeftTree, didUpdate) = innerInsert leftTree in
                                     let newLeftTime = (case leftTree of
                                            TimeLeaf -> currentTime
                                            TimeNode {} -> leftTime)
                                     in ( TimeNode
                                            { tElm = nodeElm
                                            , tId = nodeId
                                            , tFields = [(newLeftTime, newLeftTree), (rightTime, rightTree)]
                                            }
                                        , didUpdate
                                        )
                                -- Symetric for right child recursion
                                else let (newRightTree, didUpdate) = innerInsert rightTree in
                                     let newRightTime = case rightTree of
                                            TimeLeaf -> currentTime
                                            TimeNode {} -> rightTime
                                     in ( TimeNode
                                            { tElm = nodeElm
                                            , tId = nodeId
                                            , tFields = [(leftTime, leftTree), (newRightTime, newRightTree)]
                                            }
                                        , didUpdate
                                        )
    in
    
    -- Construct new tree
    let (newTree, didUpdate) = innerInsert (currentTree partialTree) in
    
    -- If there was an update, a new id was used, and the idStaticList and idCount must increase.
    let (newIdMap, newIdCount)
            = if didUpdate
                then ( [(nextId, e)], nextId + 1 )
                else ( [], nextId )
    in

    -- If tree was empty before, the new node must be root
    let newRootList =
            (case currentTree partialTree of
                TimeLeaf -> [(currentTime, nextId)]
                TimeNode {} -> []
            ) ++ (rootList partialTree)
    in
    
    -- Return new partial tree structure, with updated tree, idToStatic list, rootmap and time.
    PartialTree
        { edgeFreezer = edgeFreezer partialTree
        , idStaticList = newIdMap ++ (idStaticList partialTree)
        , rootList = newRootList
        , idCount = newIdCount
        , fieldCount = 2
        , time = currentTime + 1
        , currentTree = newTree
        }


-- Function to delete element from binary tree
delete :: Ord s => s -> PartialTree s -> PartialTree s
delete e partialTree =
    -- Fetch next id and time
    let nextId = idCount partialTree in
    let currentTime = time partialTree in

    -- Function to extract maximum element from tree
    -- Return tuple of new tree, maybe max element and new edges for the freezer
    let extractMax tree
            = case tree of
                -- If the tree is empty, then there is no maximum element to return and no edges to place in the freezer.
                TimeLeaf -> 
                        (TimeLeaf, Nothing, [])

                -- If the right subtree is empty, then the current node must be the maximum element
                TimeNode {tElm=nodeElm, tId=nodeId, tFields=[(leftTime, leftTree), (_, TimeLeaf)]} ->
                        -- Return the left tree, and current node element as max.
                        -- If the left tree is non-leaf, then freeze the left edge.
                        ( leftTree
                        , Just nodeElm
                        , case leftTree of
                                TimeLeaf -> []
                                TimeNode {tId=oldId} ->
                                    [ TimeEdge
                                        { idFrom = nodeId
                                        , field = 0
                                        , idTo = oldId
                                        , timeFrom = leftTime
                                        , timeTo = currentTime
                                        }
                                    ]
                        )

                -- Otherwise, there must be some right subtree, where the maximum element recides.
                TimeNode {tElm=nodeElm, tId=nodeId, tFields=[(leftTime, leftTree), (rightTime, rightTree @ TimeNode {tId=oldId})]} ->
                        -- Recursively extract the maximum element from the right tree
                        let (newRightTree, maxMaybe, recFrozen) = extractMax rightTree in
                        
                        -- If the node in the right subtree is max, then the right edge must be frozen, and a new time placed on the right edge.
                        -- Detect by comparing id.
                        let (newRightTime, newFrozen) =
                                case newRightTree of
                                    -- Right tree goes from node to leaf => element was extracted in the right child
                                    TimeLeaf ->
                                            ( currentTime
                                            , [TimeEdge
                                                { idFrom = nodeId
                                                , field = 1
                                                , idTo = oldId
                                                , timeFrom = rightTime
                                                , timeTo = currentTime
                                                }]
                                            )
                                    
                                    -- Otherwise, the new right tree is a node, which have an id.
                                    TimeNode {tId=newId} ->
                                            if oldId == newId
                                                then ( rightTime, [] )
                                                
                                                -- If the id is different, then the edge must be frozen
                                                else ( currentTime
                                                        , [TimeEdge
                                                            { idFrom = nodeId
                                                            , field = 1
                                                            , idTo = oldId
                                                            , timeFrom = rightTime
                                                            , timeTo = currentTime
                                                            }]
                                                        )
                        in
                        
                        -- Return node at current position, with updated right tree.
                        -- The maximum element is the one from the recursive call.
                        -- The newly frozen edges are the recursive frozen edges, and possibly the right edge of this node.
                        ( TimeNode
                            { tElm = nodeElm
                            , tId = nodeId
                            , tFields = [(leftTime, leftTree), (newRightTime, newRightTree)]
                            }
                        , maxMaybe
                        , newFrozen ++ recFrozen
                        )
    in

    -- Function to delete tree, and label time on edges
    -- Return tuple of new tree and new edges for the freezer
    let innerDelete tree
            = case tree of
                -- If the tree is a leaf, then the element does not exists, and it is therefore already deleted.
                TimeLeaf -> (TimeLeaf, [], [])

                -- If the tree is a node, then the tree paths must be checked
                TimeNode {tElm=nodeElm, tId=nodeId, tFields=[(leftTime, leftTree), (rightTime, rightTree)]} ->
                    if e == nodeElm
                        -- If the current element is equal to the element to be deleted, then the correct node is found.
                        then -- The current node is to be deleted, to both the left and right edge must be frozen.
                             -- Pattern match on the subtrees, to check if they exists.
                             let newFrozen =
                                    (case leftTree of
                                        TimeLeaf -> []
                                        TimeNode {tId=oldId} ->
                                            [ TimeEdge
                                                { idFrom = nodeId
                                                , field = 0
                                                , idTo = oldId
                                                , timeFrom = leftTime
                                                , timeTo = currentTime
                                                }
                                            ]) ++
                                    (case rightTree of
                                        TimeLeaf -> []
                                        TimeNode {tId=oldId} ->
                                            [ TimeEdge
                                                { idFrom = nodeId
                                                , field = 1
                                                , idTo = oldId
                                                , timeFrom = rightTime
                                                , timeTo = currentTime
                                                }
                                            ])
                             in
                             
                             -- The element to be placed at the current location is the predesessor, which must be the maximum element of the left tree.
                             let (newLeftTree, maxMaybe, recFrozen) = extractMax leftTree in
                             
                             -- Check if a max element was extracted
                             case maxMaybe of
                                    -- If there is no max element, then the left tree is empty.
                                    -- The right tree can therefore be inserted at the current location of the tree.
                                    -- The frozen edges can only be the right edge of the current node.
                                    Nothing -> ( rightTree, newFrozen, [] )

                                    -- There is some max element, which is then inserted at the current location, with a new id.
                                    -- Frozen elements are the edges from the current node, and the frozen edges from the max extract.
                                    -- The id to element tuple is returned.
                                    Just maxElm ->
                                            ( TimeNode
                                                { tElm = maxElm
                                                , tId = nextId
                                                , tFields = [(currentTime, newLeftTree), (currentTime, rightTree)]
                                                }
                                            , newFrozen ++ recFrozen
                                            , [(nextId, maxElm)])
                        
                        -- Otherwise, check if the element must be in the left or right tree
                        else if e < nodeElm
                                -- The element is smaller than the current element, and therefore must be in the left tree.
                                then -- Recursively update the left tree
                                     let (newLeftTree, recFrozen, idMap) = innerDelete leftTree in
                                    
                                     -- If the node below in the left tree is updated, then the left edge must be frozen, and the time updated.
                                     -- Check if different using types of the old and new left tree, and the id's
                                     let (newLeftTime, newFrozen) =
                                            case (leftTree, newLeftTree) of
                                                (TimeLeaf, TimeLeaf) ->
                                                        ( leftTime, [] )
                                                (TimeNode {tId=oldId}, TimeLeaf) ->
                                                        ( currentTime
                                                        , [TimeEdge
                                                            { idFrom = nodeId
                                                            , field = 0
                                                            , idTo = oldId
                                                            , timeFrom = leftTime
                                                            , timeTo = currentTime
                                                            }]
                                                        )
                                                (TimeNode {tId=oldId}, TimeNode {tId=newId}) ->
                                                        if oldId == newId
                                                            then ( leftTime, [] )
                                                            else ( currentTime
                                                                 , [TimeEdge
                                                                     { idFrom = nodeId
                                                                     , field = 0
                                                                     , idTo = oldId
                                                                     , timeFrom = leftTime
                                                                     , timeTo = currentTime
                                                                     }]
                                                                 )
                                     
                                     -- Return the current node, with the updated left tree.
                                     -- The frozen edges are possible the left edge, and the frozen edges from the recursive call.
                                     in (TimeNode
                                            { tElm = nodeElm
                                            , tId = nodeId
                                            , tFields = [(newLeftTime, newLeftTree), (rightTime, rightTree)]
                                            }
                                        , newFrozen ++ recFrozen
                                        , idMap)
                                
                                -- Symmetric for recursion to the right.
                                else let (newRightTree, recFrozen, idMap) = innerDelete rightTree in
                                     let (newRightTime, newFrozen) =
                                            case (rightTree, newRightTree) of
                                                (TimeLeaf, TimeLeaf) ->
                                                        ( rightTime
                                                        , []
                                                        )
                                                (TimeNode {tId=oldId}, TimeLeaf) ->
                                                        ( currentTime
                                                        , [TimeEdge
                                                            { idFrom = nodeId
                                                            , field = 1
                                                            , idTo = oldId
                                                            , timeFrom = rightTime
                                                            , timeTo = currentTime
                                                            }]
                                                        )
                                                (TimeNode {tId=oldId}, TimeNode {tId=newId}) ->
                                                        if oldId == newId
                                                            then (rightTime, [])
                                                            else ( currentTime
                                                                 , [TimeEdge
                                                                     { idFrom = nodeId
                                                                     , field = 1
                                                                     , idTo = oldId
                                                                     , timeFrom = rightTime
                                                                     , timeTo = currentTime
                                                                     }]
                                                                 )
                                     in (TimeNode
                                            { tElm = nodeElm
                                            , tId = nodeId
                                            , tFields = [(leftTime, leftTree), (newRightTime, newRightTree)]
                                            }
                                        , newFrozen ++ recFrozen
                                        , idMap)
    in
    
    -- Construct new tree
    let oldTree = currentTree partialTree in
    let (newTree, newFrozen, newIdMap) = innerDelete (oldTree) in
    
    -- Collect frozen edges
    let newEdgeFreezer = newFrozen ++ (edgeFreezer partialTree) in

    -- If root is deleted, then a new root appears
    let rootListAddition
            = case (oldTree, newTree) of
                -- No root before => no root now
                (TimeLeaf, _) ->
                    []
                -- Root before, but no root now => new root
                (_, TimeLeaf) ->
                    [(currentTime, -1)]
                -- If nodes before and after, check equality on the id, to see if there is a new root
                (TimeNode {tId=oldNodeId}, TimeNode {tId=newNodeId}) ->
                    if oldNodeId == newNodeId
                            then []
                            else [(currentTime, newNodeId)]
    in
    let newRootList = rootListAddition ++ (rootList partialTree) in

    -- Compute next id.
    -- At most one new node is created, and is so only if there is a new idMap element returned.
    let newIdCount
            = case newIdMap of
                [] -> nextId
                [(_, _)] -> nextId + 1
    in

    -- Return new partial tree structure, with updated tree, idToStatic list, rootmap and time.
    PartialTree
        { edgeFreezer = newEdgeFreezer
        , idStaticList = newIdMap ++ (idStaticList partialTree)
        , rootList = newRootList
        , idCount = newIdCount
        , fieldCount = 2
        , time = currentTime + 1
        , currentTree = newTree
        }
