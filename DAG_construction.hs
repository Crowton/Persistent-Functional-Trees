{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wall #-}

module DAG_construction where

import Data.List
import Data.Function

import qualified Data.HashMap.Strict as MH
import qualified Data.Map.Strict as MB

import DataRecords as D

import Tree_Constructor as T

import Debug.Trace



-- Fields are integers in the range [0 .. d-1]

build :: Show s => PartialTree s -> (Int -> Tree s)
build partialTree =
    -- Extract all edges in current tree
    let finish_time = time partialTree in
    let treeEdgeExtract !tree !edgeList = case tree of
            TimeLeaf -> edgeList
            TimeNode {t_id=t_id_from, t_fields=children_fields} ->
                fst (foldl (\(edgeList', field_num) (e_time_from, child) ->
                            ( treeEdgeExtract
                                child
                                ((case child of
                                    TimeLeaf -> []
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
    let innerRec !zeroOutDegree !outDegree !idToInstance = case zeroOutDegree of
            [] -> idToInstance
            id_h : ids -> 
                -- Create sorted list of outgoing edges, sorted on the time the edge starts existing
                let outgoing_edges
                        = forwards 
                          & MH.findWithDefault [] id_h
                          & concatMap (\e ->
                                let get_edges time_start edge_acc =
                                        let (time_stamp, node_at_time)
                                                = case MB.lookupGE time_start (idToInstance MH.! (id_to e)) of
                                                    Just (time_stamp, node_at_time) -> (time_stamp, node_at_time)
                                        in
                                        let time_end = min (time_to e) time_stamp in
                                        let new_edge_acc =
                                                FrozenEdge 
                                                    { field_from = field e
                                                    , node_to = node_at_time
                                                    , frozen_time_from = time_start
                                                    , frozen_time_to = time_end
                                                    }
                                                : edge_acc
                                        in
                                        if time_end == (time_to e)
                                            then new_edge_acc
                                            else get_edges time_end new_edge_acc
                                in
                                get_edges (time_from e) []
                            )
                          & sortBy (compare `on` (\e -> frozen_time_from e))
                in
                
                let node_information = idToStatic MH.! id_h in
                let num_fields = fieldCount partialTree in
                let max_fields = 2 * num_fields in

                let (time_to_instance, _, free_edges)
                        = foldl (\(time_to_instance, active_edges, free_edges) e ->
                                    let new_active_edges = MH.insert (field_from e) e active_edges in
                                    if (length free_edges) == max_fields
                                        then let split_time = frozen_time_from e in
                                            --  trace ("Creating " ++ show node_information ++ " with edge count " ++ show (length free_edges)) $
                                             let time_node = FrozenNode {staticInformation = node_information, fields = free_edges} in
                                             let new_time_to_instance = MB.insert split_time time_node time_to_instance in
                                             let new_free_edges
                                                    = active_edges
                                                    & MH.toList
                                                    & map (\(_, edge) -> edge)
                                                    & filter (\edge -> split_time < frozen_time_to edge)
                                             in
                                             ( new_time_to_instance, new_active_edges, new_free_edges )

                                        else -- trace ("Wut? " ++ show node_information ++ " with edge count " ++ show (length free_edges)) $
                                             let new_free_edges = e : free_edges in
                                             ( time_to_instance, new_active_edges, new_free_edges )
                        ) (MB.empty, MH.empty, []) outgoing_edges
                in
                
                let !last_time_to_instance
                        = case (outgoing_edges, free_edges) of
                            ([], _) -> extend
                            (_, _ : _) -> extend
                            _ -> time_to_instance
                        where extend =
                                -- trace ("Creating " ++ show node_information ++ " with edge count " ++ show (length free_edges)) $
                                let time_node = FrozenNode {staticInformation = node_information, fields = free_edges} in
                                MB.insert finish_time time_node time_to_instance
                in
                
                let !new_idToInstance = MH.insert id_h last_time_to_instance idToInstance in

                -- Update out degree of parents
                let parents = MH.findWithDefault [] id_h backwards in
                let newOutDegree = foldl (\m p -> MH.insert p (m MH.! p - 1) m) outDegree parents in
                let newZeroOut = filter (\p -> newOutDegree MH.! p == 0) parents in
                
                innerRec (newZeroOut ++ ids) newOutDegree new_idToInstance
    in
    
    -- Create id to instance
    let idToNode = innerRec init_zeroOutDegree init_outDegree MH.empty in
    
    -- Create rootmap
    let newRootList = (finish_time, -1) : (rootList partialTree) in
    let rootNodeList 
            = concatMap (\(t, r) ->
                            if r == -1
                                then [(t, Nothing)]
                                else idToNode MH.! r
                                     & MB.toList
                                     & foldl (\(from_time, acc) (to_time, node) -> (to_time, (from_time, Just (node)) : acc)) (t, [])
                                     & snd
            ) newRootList
    in
    
    -- Create root map
    let !rootMap = MB.fromDistinctDescList rootNodeList in
    
    -- Return tree constructor
    T.construct (fieldCount partialTree) rootMap
