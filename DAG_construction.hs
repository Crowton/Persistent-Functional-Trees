{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module DAG_construction where

import Data.List
import Data.Function

import qualified Data.HashMap.Strict as MH
import qualified Data.Map.Strict as MB

import DataRecords as D

import Tree_Constructor as T

import Debug.Trace
import Control.Exception -- TODO: remove imports


-- Fields are integers in the range [0 .. d-1]


-- Function to build the root list with node splits
-- Also return the number of node splits
build_root_list :: Show s => PartialTree s -> ([(Int, Maybe (FrozenNode s))], Int) -- TODO: remove show
build_root_list partialTree =
    -- Extract all edges in current tree
    let finish_time = time partialTree in
    
    -- Recursively extract edges to the edgelist
    let treeEdgeExtract tree edgeList = case tree of
            -- No edges span from leaves
            TimeLeaf -> edgeList

            -- Edges may span from node to child node
            TimeNode {t_id=t_id_from, t_fields=children_fields} ->
                fst (foldl (\(edgeList', field_num) (e_time_from, child) ->
                            -- Recurse into child, with updated edge list
                            ( treeEdgeExtract
                                child
                                ((case child of
                                    -- Child is leaf => no edge
                                    TimeLeaf -> []

                                    -- Child is node => edge
                                    TimeNode {t_id=t_id_to} ->
                                        -- trace ("Edge " ++ (show (t_id_from, field_num)) ++ " -> " ++ (show t_id_to) ++ ", time [" ++ (show e_time_from) ++ "," ++ (show finish_time) ++ "[") $
                                        [ TimeEdge
                                            { id_from = t_id_from
                                            , field = field_num
                                            , id_to = t_id_to
                                            , time_from = e_time_from
                                            , time_to = finish_time
                                            }
                                        ]
                                 )
                                ++ edgeList')
                            , field_num + 1)
                ) (edgeList, 0) children_fields)
    in
    -- trace ("Edges before: " ++ show (edgeFreezer partialTree)) $
    let edgeList = treeEdgeExtract (currentTree partialTree) (edgeFreezer partialTree) in
    let edge_count = length edgeList in
    -- trace ("Edges after: " ++ show edgeList) $
    -- trace ("Total number of edges: " ++ (show (length edgeList))) $

    -- Map from id to static information
    let idToStatic = MH.fromList (idStaticList partialTree) in

    -- Forwards and Backwards incidence lists
    let forwards
            = foldl (\m e ->
                    let edges = MH.findWithDefault [] (id_from e) m in
                    MH.insert (id_from e) (e : edges) m
            ) MH.empty edgeList in
    let backwards
            = foldl (\m e ->
                let edges = MH.findWithDefault [] (id_to e) m in
                MH.insert (id_to e) (id_from e : edges) m
            ) MH.empty edgeList in

    -- Current out degree of each node
    let init_outDegree = MH.map length forwards in

    -- Nodes with zero out degree
    let init_zeroOutDegree = filter (\i -> not (MH.member i init_outDegree)) [0 .. idCount partialTree - 1] in
    
    -- trace ("Forward edges: " ++ show forwards) $
    -- trace ("Backwards edges: " ++ show backwards) $
    -- trace ("Initial out-degree: " ++ show init_outDegree) $
    -- trace ("Initial zero out-degree: " ++ show init_zeroOutDegree) $
    -- trace "" $

    -- Function to create DAG, by processing nodes of zero out degree
    let innerRec node_splits zeroOutDegree outDegree idToInstance = case zeroOutDegree of
            [] -> (idToInstance, node_splits)
            id_h : ids ->
                -- trace ("Processing id: " ++ show id_h) $
                -- Create sorted list of outgoing edges, sorted on the time the edge starts existing
                let outgoing_edges
                        = forwards
                          & MH.findWithDefault [] id_h
                          & foldl (\edge_acc e ->
                                -- Recursively get the nodes an edge spans over in time
                                let get_edges time_start inner_edge_acc =
                                        -- There must always be a node that the current start time of the edge points to
                                        -- The time_stamp is the time this instance dies
                                        let (time_stamp, node_at_time)
                                                -- Use GT as map is from node die time, and hitting on a time should then return the next node
                                                = case MB.lookupGT time_start (idToInstance MH.! id_to e) of
                                                    Just (time_stamp, node_at_time) -> (time_stamp, node_at_time)
                                        in
                                        -- The end time of the current edge to node must be the min of the node die time and edge die time
                                        let time_end = min (time_to e) time_stamp in
                                        -- This results in the edge from start to end pointing to the found instance
                                        let new_edge_acc =
                                                FrozenEdge
                                                    { field_from = field e
                                                    , node_to = node_at_time
                                                    , frozen_time_from = time_start
                                                    , frozen_time_to = time_end
                                                    }
                                                : inner_edge_acc
                                        in
                                        
                                        if time_end == time_to e
                                            -- If time end of the frozen edge is the end tim eof the whole edge, then all node instances are found
                                            then new_edge_acc
                                            -- Otherwise recurse with the new starting time being the current end time
                                            else get_edges time_end new_edge_acc
                                in
                                get_edges (time_from e) edge_acc
                            ) []
                          & sortBy (compare `on` frozen_time_from)
                in
                
                -- trace ("Out-going count: " ++ (show (length outgoing_edges))) $
                -- trace ("Out-going edges: " ++ show outgoing_edges) $
                
                -- Fetch base information for creating the time to instance of the current node
                let node_information = idToStatic MH.! id_h in
                let num_fields = fieldCount partialTree in
                let max_fields = 2 * num_fields in
                
                -- Sweepline over the out going edges of the node
                let (time_to_instance, _, free_edges)
                        -- The time_to_instance is the map to be added to the idToInstance global map
                        -- The active_edges is a map from field to the latest edge of that field, which is the active candidate
                        -- The free_edges are the edges currently needing a node instance
                        = foldl (\(time_to_instance, active_edges, free_edges) e ->
                                    -- Update active_edges with the current edge
                                    -- assert (MH.size active_edges <= num_fields) $
                                    let new_active_edges = MH.insert (field_from e) e active_edges in
                                    
                                    if length free_edges == max_fields
                                        -- If the current free_edges are max, then a new node must be build
                                        then -- The time of the split is the start time of the current node
                                             let split_time = frozen_time_from e in
                                             -- The node instance contains the static information and the free edges
                                             let time_node = FrozenNode {staticInformation = node_information, fields = free_edges} in
                                             -- Insert the instance at the split time
                                             let new_time_to_instance = MB.insert split_time time_node time_to_instance in
                                             -- The new free edges are the edges from active edges, which spans across the split time
                                             -- As they are inserted in sorted order, the only check is that the end time is strictly after the split time
                                             let new_free_edges
                                                    = new_active_edges
                                                    & MH.toList
                                                    & map snd
                                                    & filter (\edge -> split_time < frozen_time_to edge)
                                             in
                                             ( new_time_to_instance, new_active_edges, new_free_edges )

                                        -- Otherwise, there is room in the free edges list, which the current edge is added to
                                        else let new_free_edges = e : free_edges in
                                             ( time_to_instance, new_active_edges, new_free_edges )
                        ) (MB.empty, MH.empty, []) outgoing_edges
                in
                
                -- If the node have no outgoing edges or there is free edges left, then a final instance is to be made
                -- The die time is set to the die time of the structure, as only an upper bound for the last instance is needed
                let last_time_to_instance
                        = case (outgoing_edges, free_edges) of
                            ([], _) -> extend
                            (_, _ : _) -> extend
                            _ -> time_to_instance
                        where extend =
                                let time_node = FrozenNode {staticInformation = node_information, fields = free_edges} in
                                MB.insert finish_time time_node time_to_instance
                in
                
                -- New node splits is the number of nodes created - 1
                -- let new_node_splits = (MB.size last_time_to_instance) in
                let new_node_splits = (MB.size last_time_to_instance) - 1 in
                
                -- assert (
                --     last_time_to_instance
                --     & MB.toList
                --     & map (\(_, FrozenNode {fields = edges}) -> ((length edges) <= max_fields))
                --     & all (\x -> x)
                -- ) $
                
                -- trace ("Node created count: " ++ (show (MB.size last_time_to_instance))) $
                -- trace ("Nodes created: " ++ show last_time_to_instance) $

                -- Update the idToInstance map with the intance map of the current node id
                let new_idToInstance = MH.insert id_h last_time_to_instance idToInstance in

                -- Update out degree of parents
                let parents = MH.findWithDefault [] id_h backwards in
                let newOutDegree = foldl (\m p -> MH.insert p (m MH.! p - 1) m) outDegree parents in
                let newZeroOut = filter (\p -> newOutDegree MH.! p == 0) parents in
                
                -- trace ("Parents: " ++ show parents) $
                -- trace ("New out-degree: " ++ show newOutDegree) $
                -- trace ("New zero out: " ++ show newZeroOut) $
                -- trace "" $

                -- Recurse with the updated zero out degree and idToInstance
                innerRec (node_splits + new_node_splits) (newZeroOut ++ ids) newOutDegree new_idToInstance
    in

    -- Create id to instance
    -- Maps from id to map over node end time and instance
    let (idToNode, node_splits) = innerRec (0::Int) init_zeroOutDegree init_outDegree MH.empty in

    -- assert (node_splits <= edge_count * 2) $

    -- Create rootmap
    let newRootList = (finish_time, -1) : rootList partialTree in
    ( concatMap (\(t, r) ->
                    if r == -1
                        then [(t, Nothing)]
                        else idToNode MH.! r
                                & MB.toList
                                & foldl (\(from_time, acc) (to_time, node) -> (to_time, (from_time, Just node) : acc)) (t, [])
                                & snd
                ) newRootList
    -- , edge_count
    , node_splits
    )


-- Function to build the tree with node splits
build :: Show s => PartialTree s -> (Int -> Tree s) -- TODO: remove show
build partialTree =
    -- Build root list with the above function
    let (rootNodeList, _) = build_root_list partialTree in

    -- Create root map
    let rootMap = MB.fromDistinctDescList rootNodeList in

    -- Return tree constructor
    T.construct (fieldCount partialTree) rootMap


-- Function to build the tree without node splits
build_non_split :: PartialTree s -> (Int -> Tree s)
build_non_split partialTree =
    -- Extract all edges in current tree
    let finish_time = time partialTree in
    
    -- Recursively extract edges to the edgelist
    let treeEdgeExtract tree edgeList = case tree of
            -- No edges span from leaves
            TimeLeaf -> edgeList

            -- Edges may span from node to child node
            TimeNode {t_id=t_id_from, t_fields=children_fields} ->
                fst (foldl (\(edgeList', field_num) (e_time_from, child) ->
                            -- Recurse into child, with updated edge list
                            ( treeEdgeExtract
                                child
                                ((case child of
                                    -- Child is leaf => no edge
                                    TimeLeaf -> []

                                    -- Child is node => edge
                                    TimeNode {t_id=t_id_to} ->
                                        [ TimeEdge
                                            { id_from = t_id_from
                                            , field = field_num
                                            , id_to = t_id_to
                                            , time_from = e_time_from
                                            , time_to = finish_time
                                            }
                                        ]
                                 )
                                ++ edgeList')
                            , field_num + 1)
                ) (edgeList, 0) children_fields)
    in
    let edgeList = treeEdgeExtract (currentTree partialTree) (edgeFreezer partialTree) in

    -- Map from id to static information
    let idToStatic = MH.fromList (idStaticList partialTree) in

    -- Forwards and Bcakwards incidense lists
    let forwards
            = foldl (\m e ->
                    let edges = MH.findWithDefault [] (id_from e) m in
                    MH.insert (id_from e) (e : edges) m
            ) MH.empty edgeList in
    let backwards
            = foldl (\m e ->
                let edges = MH.findWithDefault [] (id_to e) m in
                MH.insert (id_to e) (id_from e : edges) m
            ) MH.empty edgeList in

    -- Current out degree of each node
    let init_outDegree = MH.map length forwards in

    -- Nodes with zero out degree
    let init_zeroOutDegree = filter (\i -> not (MH.member i init_outDegree)) [0 .. (idCount partialTree) - 1] in

    -- Function to create DAG, by processing nodes of zero out degree
    let innerRec zeroOutDegree outDegree idToInstance = case zeroOutDegree of
            [] -> idToInstance
            id_h : ids ->
                -- Create fields by converting edges to frozen edges with node instances
                let node_fields
                        = map (\e ->
                            FrozenEdge
                                { field_from = field e
                                , node_to = idToInstance MH.! (id_to e)
                                , frozen_time_from = time_from e
                                , frozen_time_to = time_to e
                                }
                        ) (MH.findWithDefault [] id_h forwards) in
                -- Create node instance
                let node = FrozenNode {staticInformation = idToStatic MH.! id_h, fields = node_fields} in

                -- Update out degree of parents
                let parents = MH.findWithDefault [] id_h backwards in
                let newOutDegree = foldl (\m p -> MH.insert p (m MH.! p - 1) m) outDegree parents in
                let newZeroOut = filter (\p -> newOutDegree MH.! p == 0) parents in
                
                -- Recurse with the updated zero out degree and idToInstance
                innerRec (newZeroOut ++ ids) newOutDegree (MH.insert id_h node idToInstance)
    in

    -- Create id to instance
    let idToNode = innerRec init_zeroOutDegree init_outDegree MH.empty in

    -- Create rootmap
    let newRootList = (finish_time, -1) : (rootList partialTree) in
    let rootNodeList = map (\(t, r) -> (t, if r == -1 then Nothing else Just (idToNode MH.! r))) newRootList in

    -- Create root map
    let rootMap = MB.fromDistinctDescList rootNodeList in

    -- Return tree constructor
    T.construct (fieldCount partialTree) rootMap
