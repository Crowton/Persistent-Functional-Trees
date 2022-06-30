{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module DAGconstruction where

import Data.List
import Data.Function

import qualified Data.HashMap.Strict as MH
import qualified Data.Map.Strict as MB

import DataRecords as D

import TreeConstructor as T

-- Fields are integers in the range [0 .. d-1]


-- Function to build the root list with node splits
-- Also return the number of node splits
buildRootList :: PartialTree s -> ([(Int, Maybe (FrozenNode s))], Int)
buildRootList partialTree =
    -- Extract all edges in current tree
    let finishTime = time partialTree in
    
    -- Recursively extract edges to the edgelist
    let treeEdgeExtract tree edgeList = case tree of
            -- No edges span from leaves
            TimeLeaf -> edgeList

            -- Edges may span from node to child node
            TimeNode {tId=tIdFrom, tFields=childrenFields} ->
                fst (foldl (\(edgeList', fieldNum) (eTimeFrom, child) ->
                            -- Recurse into child, with updated edge list
                            ( treeEdgeExtract
                                child
                                ((case child of
                                    -- Child is leaf => no edge
                                    TimeLeaf -> []

                                    -- Child is node => edge
                                    TimeNode {tId=tIdTo} ->
                                        [ TimeEdge
                                            { idFrom = tIdFrom
                                            , field = fieldNum
                                            , idTo = tIdTo
                                            , timeFrom = eTimeFrom
                                            , timeTo = finishTime
                                            }
                                        ]
                                 )
                                ++ edgeList')
                            , fieldNum + 1)
                ) (edgeList, 0) childrenFields)
    in
    let edgeList = treeEdgeExtract (currentTree partialTree) (edgeFreezer partialTree) in

    -- Map from id to static information
    let idToStatic = MH.fromList (idStaticList partialTree) in

    -- Forwards and Backwards incidence lists
    let forwards
            = foldl (\m e ->
                    let edges = MH.findWithDefault [] (idFrom e) m in
                    MH.insert (idFrom e) (e : edges) m
            ) MH.empty edgeList in
    let backwards
            = foldl (\m e ->
                let edges = MH.findWithDefault [] (idTo e) m in
                MH.insert (idTo e) (idFrom e : edges) m
            ) MH.empty edgeList in

    -- Current out degree of each node
    let initOutDegree = MH.map length forwards in

    -- Nodes with zero out degree
    let initZeroOutDegree = filter (\i -> not (MH.member i initOutDegree)) [0 .. idCount partialTree - 1] in
    
    -- Function to create DAG, by processing nodes of zero out degree
    let innerRec nodeSplits zeroOutDegree outDegree idToInstance = case zeroOutDegree of
            [] -> (idToInstance, nodeSplits)
            idH : ids ->
                -- Create sorted list of outgoing edges, sorted on the time the edge starts existing
                let outgoingEdges
                        = forwards
                          & MH.findWithDefault [] idH
                          & foldl (\edgeAcc e ->
                                -- Recursively get the nodes an edge spans over in time
                                let getEdges timeStart innerEdgeAcc =
                                        -- There must always be a node that the current start time of the edge points to
                                        -- The timeStamp is the time this instance dies
                                        let (timeStamp, nodeAtTime)
                                                -- Use GT as map is from node die time, and hitting on a time should then return the next node
                                                = case MB.lookupGT timeStart (idToInstance MH.! idTo e) of
                                                    Just (timeStamp, nodeAtTime) -> (timeStamp, nodeAtTime)
                                        in
                                        -- The end time of the current edge to node must be the min of the node die time and edge die time
                                        let timeEnd = min (timeTo e) timeStamp in
                                        -- This results in the edge from start to end pointing to the found instance
                                        let newEdgeAcc =
                                                FrozenEdge
                                                    { fieldFrom = field e
                                                    , nodeTo = nodeAtTime
                                                    , frozenTimeFrom = timeStart
                                                    , frozenTimeTo = timeEnd
                                                    }
                                                : innerEdgeAcc
                                        in
                                        
                                        if timeEnd == timeTo e
                                            -- If time end of the frozen edge is the end tim eof the whole edge, then all node instances are found
                                            then newEdgeAcc
                                            -- Otherwise recurse with the new starting time being the current end time
                                            else getEdges timeEnd newEdgeAcc
                                in
                                getEdges (timeFrom e) edgeAcc
                            ) []
                          & sortBy (compare `on` frozenTimeFrom)
                in
                
                -- Fetch base information for creating the time to instance of the current node
                let nodeInformation = idToStatic MH.! idH in
                let numFields = fieldCount partialTree in
                let maxFields = 2 * numFields in
                
                -- Sweepline over the out going edges of the node
                let (timeToInstance, _, freeEdges)
                        -- The timeToInstance is the map to be added to the idToInstance global map
                        -- The activeEdges is a map from field to the latest edge of that field, which is the active candidate
                        -- The freeEdges are the edges currently needing a node instance
                        = foldl (\(timeToInstance, activeEdges, freeEdges) e ->
                                    -- Update activeEdges with the current edge
                                    let newActiveEdges = MH.insert (fieldFrom e) e activeEdges in
                                    
                                    if length freeEdges == maxFields
                                        -- If the current freeEdges are max, then a new node must be build
                                        then -- The time of the split is the start time of the current node
                                             let splitTime = frozenTimeFrom e in
                                             -- The node instance contains the static information and the free edges
                                             let timeNode = FrozenNode {staticInformation = nodeInformation, fields = freeEdges} in
                                             -- Insert the instance at the split time
                                             let newTimeToInstance = MB.insert splitTime timeNode timeToInstance in
                                             -- The new free edges are the edges from active edges, which spans across the split time
                                             -- As they are inserted in sorted order, the only check is that the end time is strictly after the split time
                                             let newFreeEdges
                                                    = newActiveEdges
                                                    & MH.toList
                                                    & map snd
                                                    & filter (\edge -> splitTime < frozenTimeTo edge)
                                             in
                                             ( newTimeToInstance, newActiveEdges, newFreeEdges )

                                        -- Otherwise, there is room in the free edges list, which the current edge is added to
                                        else let newFreeEdges = e : freeEdges in
                                             ( timeToInstance, newActiveEdges, newFreeEdges )
                        ) (MB.empty, MH.empty, []) outgoingEdges
                in
                
                -- If the node have no outgoing edges or there is free edges left, then a final instance is to be made
                -- The die time is set to the die time of the structure, as only an upper bound for the last instance is needed
                let lastTimeToInstance
                        = case (outgoingEdges, freeEdges) of
                            ([], _) -> extend
                            (_, _ : _) -> extend
                            _ -> timeToInstance
                        where extend =
                                let timeNode = FrozenNode {staticInformation = nodeInformation, fields = freeEdges} in
                                MB.insert finishTime timeNode timeToInstance
                in
                
                -- New node splits is the number of nodes created - 1
                let newNodeSplits = (MB.size lastTimeToInstance) - 1 in
                
                -- Update the idToInstance map with the intance map of the current node id
                let newIdToInstance = MH.insert idH lastTimeToInstance idToInstance in

                -- Update out degree of parents
                let parents = MH.findWithDefault [] idH backwards in
                let newOutDegree = foldl (\m p -> MH.insert p (m MH.! p - 1) m) outDegree parents in
                let newZeroOut = filter (\p -> newOutDegree MH.! p == 0) parents in

                -- Recurse with the updated zero out degree and idToInstance
                innerRec (nodeSplits + newNodeSplits) (newZeroOut ++ ids) newOutDegree newIdToInstance
    in

    -- Create id to instance
    -- Maps from id to map over node end time and instance
    let (idToNode, nodeSplits) = innerRec (0::Int) initZeroOutDegree initOutDegree MH.empty in

    -- Create rootmap
    let newRootList = (finishTime, -1) : rootList partialTree in
    ( concatMap (\(t, r) ->
                    if r == -1
                        then [(t, Nothing)]
                        else idToNode MH.! r
                                & MB.toList
                                & foldl (\(fromTime, acc) (toTime, node) -> (toTime, (fromTime, Just node) : acc)) (t, [])
                                & snd
                ) newRootList
    , nodeSplits
    )


-- Function to build the tree with node splits
build :: PartialTree s -> (Int -> Tree s)
build partialTree =
    -- Build root list with the above function
    let (rootNodeList, _) = buildRootList partialTree in

    -- Create root map
    let rootMap = MB.fromDistinctDescList rootNodeList in

    -- Return tree constructor
    T.construct (fieldCount partialTree) rootMap


-- Function to build the tree without node splits
buildNonSplit :: PartialTree s -> (Int -> Tree s)
buildNonSplit partialTree =
    -- Extract all edges in current tree
    let finishTime = time partialTree in
    
    -- Recursively extract edges to the edgelist
    let treeEdgeExtract tree edgeList = case tree of
            -- No edges span from leaves
            TimeLeaf -> edgeList

            -- Edges may span from node to child node
            TimeNode {tId=tIdFrom, tFields=childrenFields} ->
                fst (foldl (\(edgeList', fieldNum) (eTimeFrom, child) ->
                            -- Recurse into child, with updated edge list
                            ( treeEdgeExtract
                                child
                                ((case child of
                                    -- Child is leaf => no edge
                                    TimeLeaf -> []

                                    -- Child is node => edge
                                    TimeNode {tId=tIdTo} ->
                                        [ TimeEdge
                                            { idFrom = tIdFrom
                                            , field = fieldNum
                                            , idTo = tIdTo
                                            , timeFrom = eTimeFrom
                                            , timeTo = finishTime
                                            }
                                        ]
                                 )
                                ++ edgeList')
                            , fieldNum + 1)
                ) (edgeList, 0) childrenFields)
    in
    let edgeList = treeEdgeExtract (currentTree partialTree) (edgeFreezer partialTree) in

    -- Map from id to static information
    let idToStatic = MH.fromList (idStaticList partialTree) in

    -- Forwards and Bcakwards incidense lists
    let forwards
            = foldl (\m e ->
                    let edges = MH.findWithDefault [] (idFrom e) m in
                    MH.insert (idFrom e) (e : edges) m
            ) MH.empty edgeList in
    let backwards
            = foldl (\m e ->
                let edges = MH.findWithDefault [] (idTo e) m in
                MH.insert (idTo e) (idFrom e : edges) m
            ) MH.empty edgeList in

    -- Current out degree of each node
    let initOutDegree = MH.map length forwards in

    -- Nodes with zero out degree
    let initZeroOutDegree = filter (\i -> not (MH.member i initOutDegree)) [0 .. (idCount partialTree) - 1] in

    -- Function to create DAG, by processing nodes of zero out degree
    let innerRec zeroOutDegree outDegree idToInstance = case zeroOutDegree of
            [] -> idToInstance
            idH : ids ->
                -- Create fields by converting edges to frozen edges with node instances
                let nodeFields
                        = map (\e ->
                            FrozenEdge
                                { fieldFrom = field e
                                , nodeTo = idToInstance MH.! (idTo e)
                                , frozenTimeFrom = timeFrom e
                                , frozenTimeTo = timeTo e
                                }
                        ) (MH.findWithDefault [] idH forwards) in
                -- Create node instance
                let node = FrozenNode {staticInformation = idToStatic MH.! idH, fields = nodeFields} in

                -- Update out degree of parents
                let parents = MH.findWithDefault [] idH backwards in
                let newOutDegree = foldl (\m p -> MH.insert p (m MH.! p - 1) m) outDegree parents in
                let newZeroOut = filter (\p -> newOutDegree MH.! p == 0) parents in
                
                -- Recurse with the updated zero out degree and idToInstance
                innerRec (newZeroOut ++ ids) newOutDegree (MH.insert idH node idToInstance)
    in

    -- Create id to instance
    let idToNode = innerRec initZeroOutDegree initOutDegree MH.empty in

    -- Create rootmap
    let newRootList = (finishTime, -1) : (rootList partialTree) in
    let rootNodeList = map (\(t, r) -> (t, if r == -1 then Nothing else Just (idToNode MH.! r))) newRootList in

    -- Create root map
    let rootMap = MB.fromDistinctDescList rootNodeList in

    -- Return tree constructor
    T.construct (fieldCount partialTree) rootMap
