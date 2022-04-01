{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wall #-}

module Binary_Tree_persistent_mock where

import DataRecords

-- import Prettify
import Debug.Trace


construct_empty_tree :: PartialTree s
construct_empty_tree =
    PartialTree {
        edgeFreezer = [],
        idStaticList = [],
        rootList = [(0, -1)],
        idCount = 0,
        fieldCount = 2,
        time = 1,
        currentTree = TimeLeaf
    }


insert :: Ord s => s -> PartialTree s -> PartialTree s
insert e partialTree =
    -- Fetch next id and time
    let next_id = idCount partialTree in
    let current_time = time partialTree in
    
    -- Function to insert tree, and label time on edges
    let inner_insert tree
            = case tree of
                TimeLeaf ->
                    TimeNode
                        { t_elm = e
                        , t_id = next_id
                        , t_fields = [(current_time, TimeLeaf), (current_time, TimeLeaf)]
                        }
                TimeNode {t_elm=node_elm, t_id=node_id, t_fields=[(left_time, left_tree), (right_time, right_tree)]} ->
                    if e == node_elm
                        then tree
                        else if e < node_elm
                                then let new_left_time = (case left_tree of
                                            TimeLeaf -> current_time
                                            TimeNode {} -> left_time)
                                     in TimeNode
                                            { t_elm = node_elm
                                            , t_id = node_id
                                            , t_fields = [(new_left_time, inner_insert left_tree), (right_time, right_tree)]
                                            }
                                else let new_right_time = case right_tree of
                                            TimeLeaf -> current_time
                                            TimeNode {} -> right_time
                                     in TimeNode
                                            { t_elm = node_elm
                                            , t_id = node_id
                                            , t_fields = [(left_time, left_tree), (new_right_time, inner_insert right_tree)]
                                            }
                _ -> error "Not reachable"
    in
    
    -- Construct new tree
    let new_tree = inner_insert (currentTree partialTree) in
    
    -- If tree was empty before, the new node must be root
    let new_rootList =
            (case currentTree partialTree of
                TimeLeaf -> [(current_time, next_id)]
                TimeNode {} -> []
            ) ++ (rootList partialTree)
    in
    
    -- TODO: detect if there was a chance and not update id's and stuff
    PartialTree
        { edgeFreezer = edgeFreezer partialTree
        , idStaticList = (next_id, e) : (idStaticList partialTree)
        , rootList = new_rootList
        , idCount = next_id + 1
        , fieldCount = 2
        , time = current_time + 1
        , currentTree = new_tree
        }


delete :: Show s => Ord s => s -> PartialTree s -> PartialTree s
delete e partialTree =
    -- Fetch next id and time
    let next_id = idCount partialTree in
    let current_time = time partialTree in

    -- Function to delete tree, and label time on edges
    -- Return tuple of new tree and new edges for the freezer
    -- TODO: refracter this shit
    let inner_delete tree
            = case tree of
                TimeLeaf -> (TimeLeaf, [])
                TimeNode {t_elm=node_elm, t_id=node_id, t_fields=[(left_time, left_tree), (right_time, right_tree)]} ->
                    if e == node_elm
                        then ( TimeLeaf
                             , (case left_tree of
                                 TimeLeaf -> []
                                 TimeNode {t_id=old_id} ->
                                     [ TimeEdge
                                        { id_from = node_id
                                        , field = 0
                                        , id_to = old_id
                                        , time_from = left_time
                                        , time_to = current_time
                                        }
                                     ]) ++
                               (case right_tree of
                                 TimeLeaf -> []
                                 TimeNode {t_id=old_id} ->
                                     [ TimeEdge
                                        { id_from = node_id
                                        , field = 1
                                        , id_to = old_id
                                        , time_from = right_time
                                        , time_to = current_time
                                        }
                                     ])
                             )
                        else if e < node_elm
                                then let (new_left_tree, rec_frozen) = inner_delete left_tree in
                                     let (new_left_time, new_frozen) =
                                            case (left_tree, new_left_tree) of
                                                (TimeLeaf, TimeLeaf) ->
                                                        ( left_time
                                                        , []
                                                        )
                                                (TimeNode {t_id=old_id}, TimeLeaf) ->
                                                        ( current_time
                                                        , [TimeEdge
                                                            { id_from = node_id
                                                            , field = 0
                                                            , id_to = old_id
                                                            , time_from = left_time
                                                            , time_to = current_time
                                                            }]
                                                        )
                                                (TimeNode {t_id=old_id}, TimeNode {t_id=new_id}) ->
                                                        if old_id == new_id
                                                            then (left_time, [])
                                                            else ( current_time
                                                                 , [TimeEdge
                                                                     { id_from = node_id
                                                                     , field = 0
                                                                     , id_to = old_id
                                                                     , time_from = left_time
                                                                     , time_to = current_time
                                                                     }]
                                                                 )
                                     in (TimeNode
                                            { t_elm = node_elm
                                            , t_id = node_id
                                            , t_fields = [(new_left_time, new_left_tree), (right_time, right_tree)]
                                            }
                                        , new_frozen ++ rec_frozen)
                                else let (new_right_tree, rec_frozen) = inner_delete right_tree in
                                     let (new_right_time, new_frozen) =
                                            case (right_tree, new_right_tree) of
                                                (TimeLeaf, TimeLeaf) ->
                                                        ( right_time
                                                        , []
                                                        )
                                                (TimeNode {t_id=old_id}, TimeLeaf) ->
                                                        ( current_time
                                                        , [TimeEdge
                                                            { id_from = node_id
                                                            , field = 1
                                                            , id_to = old_id
                                                            , time_from = right_time
                                                            , time_to = current_time
                                                            }]
                                                        )
                                                (TimeNode {t_id=old_id}, TimeNode {t_id=new_id}) ->
                                                        if old_id == new_id
                                                            then (right_time, [])
                                                            else ( current_time
                                                                 , [TimeEdge
                                                                     { id_from = node_id
                                                                     , field = 1
                                                                     , id_to = old_id
                                                                     , time_from = right_time
                                                                     , time_to = current_time
                                                                     }]
                                                                 )
                                     in (TimeNode
                                            { t_elm = node_elm
                                            , t_id = node_id
                                            , t_fields = [(left_time, left_tree), (new_right_time, new_right_tree)]
                                            }
                                        , new_frozen ++ rec_frozen)
                _ -> error "Not reachable"
    in
    
    -- Construct new tree
    let old_tree = currentTree partialTree in
    let (new_tree, new_frozen) = inner_delete (old_tree) in
    
    -- Collect frozen edges
    let new_edgeFreezer = new_frozen ++ (edgeFreezer partialTree) in

    -- If root is deleted, then a new root appears
    let rootList_addition
            = case (old_tree, new_tree) of
                (TimeLeaf, _) ->
                    [] -- No root before => no root now
                (_, TimeLeaf) ->
                    [(current_time, -1)]  -- Root before, but no root now => new root
                (TimeNode {t_id=old_node_id}, TimeNode {t_id=new_node_id}) ->
                    if old_node_id == new_node_id
                            then []
                            else [(current_time, new_node_id)]
    in

    let new_rootList = rootList_addition ++ (rootList partialTree) in

    -- TODO: detect if there was a chance and not update id's and stuff
    PartialTree
        { edgeFreezer = new_edgeFreezer
        , idStaticList = (next_id, e) : (idStaticList partialTree)
        , rootList = new_rootList
        , idCount = next_id + 1
        , fieldCount = 2
        , time = current_time + 1
        , currentTree = new_tree
        }