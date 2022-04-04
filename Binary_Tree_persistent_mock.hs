{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wall #-}

module Binary_Tree_persistent_mock where

import DataRecords


-- Function to construct inital empty tree.
-- There are no frozen edges and id's, the time is 0 and the degree 2.
-- Initially, the empty tree is the root.
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


-- Function to insert element into binary tree
insert :: Ord s => s -> PartialTree s -> PartialTree s
insert e partialTree =
    -- Fetch next id and time
    let next_id = idCount partialTree in
    let current_time = time partialTree in
    
    -- Function to insert tree, and label time on edges
    let --inner_insert :: TimeTree s -> (TimeTree s, Bool)
        inner_insert tree
            = case tree of
                -- Empty node is where the element is to be inserted.
                -- Create new node, with two leaf children, recording the current time.
                TimeLeaf ->
                    ( TimeNode
                        { t_elm = e
                        , t_id = next_id
                        , t_fields = [(current_time, TimeLeaf), (current_time, TimeLeaf)]
                        }
                    , True
                    )
                
                -- On a node, the element in the node and the new element must be compared
                TimeNode {t_elm=node_elm, t_id=node_id, t_fields=[(left_time, left_tree), (right_time, right_tree)]} ->
                    if e == node_elm
                        -- The elements are the same => element already exists, so return the same tree
                        then ( tree, False )

                        -- Else check which child to recurse into
                        else if e < node_elm
                                -- Smaller => left child recursion
                                -- If left child was leaf before, then the new element is inserted here. Update time on edge
                                then let (new_left_tree, did_update) = inner_insert left_tree in
                                     let new_left_time = (case left_tree of
                                            TimeLeaf -> current_time
                                            TimeNode {} -> left_time)
                                     in ( TimeNode
                                            { t_elm = node_elm
                                            , t_id = node_id
                                            , t_fields = [(new_left_time, new_left_tree), (right_time, right_tree)]
                                            }
                                        , did_update
                                        )
                                -- Symetric for right child recursion
                                else let (new_right_tree, did_update) = inner_insert right_tree in
                                     let new_right_time = case right_tree of
                                            TimeLeaf -> current_time
                                            TimeNode {} -> right_time
                                     in ( TimeNode
                                            { t_elm = node_elm
                                            , t_id = node_id
                                            , t_fields = [(left_time, left_tree), (new_right_time, new_right_tree)]
                                            }
                                        , did_update
                                        )
    in
    
    -- Construct new tree
    let (new_tree, did_update) = inner_insert (currentTree partialTree) in
    
    -- If there was an update, a new id was used, and the idStaticList and idCount must increase.
    let (new_id_map, new_idCount)
            = if did_update
                then ( [(next_id, e)], next_id + 1 )
                else ( [], next_id )
    in

    -- If tree was empty before, the new node must be root
    let new_rootList =
            (case currentTree partialTree of
                TimeLeaf -> [(current_time, next_id)]
                TimeNode {} -> []
            ) ++ (rootList partialTree)
    in
    
    -- Return new partial tree structure, with updated tree, idToStatic list, rootmap and time.
    PartialTree
        { edgeFreezer = edgeFreezer partialTree
        , idStaticList = new_id_map ++ (idStaticList partialTree)
        , rootList = new_rootList
        , idCount = new_idCount
        , fieldCount = 2
        , time = current_time + 1
        , currentTree = new_tree
        }


-- Function to delete element from binary tree
delete :: Show s => Ord s => s -> PartialTree s -> PartialTree s
delete e partialTree =
    -- Fetch next id and time
    let next_id = idCount partialTree in
    let current_time = time partialTree in

    -- Function to extract maximum element from tree
    -- Return tuple of new tree, maybe max element and new edges for the freezer
    let --extract_max :: TimeTree s -> (TimeTree s, Maybe s, [TimeEdge])
        extract_max tree
            = case tree of
                -- If the tree is empty, then there is no maximum element to return and no edges to place in the freezer.
                TimeLeaf -> 
                        (TimeLeaf, Nothing, [])

                -- If the right subtree is empty, then the current node must be the maximum element
                TimeNode {t_elm=node_elm, t_id=node_id, t_fields=[(left_time, left_tree), (right_time, TimeLeaf)]} ->
                        -- Return the left tree, and current node element as max.
                        -- If the left tree is non-leaf, then freeze the left edge.
                        ( left_tree
                        , Just node_elm
                        , case left_tree of
                                TimeLeaf -> []
                                TimeNode {t_id=old_id} ->
                                    [ TimeEdge
                                        { id_from = node_id
                                        , field = 0
                                        , id_to = old_id
                                        , time_from = left_time
                                        , time_to = current_time
                                        }
                                    ]
                        )

                -- Otherwise, there must be some right subtree, where the maximum element recides.
                TimeNode {t_elm=node_elm, t_id=node_id, t_fields=[(left_time, left_tree), (right_time, right_tree @ TimeNode {t_id=old_id})]} ->
                        -- Recursively extract the maximum element from the right tree
                        let (new_right_tree, max_maybe, rec_frozen) = extract_max right_tree in
                        
                        -- If the node in the right subtree is max, then the right edge must be frozen, and a new time placed on the right edge.
                        -- Detect by comparing id.
                        let (new_right_time, new_frozen) =
                                case new_right_tree of
                                    -- Right tree goes from node to leaf => element was extracted in the right child
                                    TimeLeaf ->
                                            ( current_time
                                            , [TimeEdge
                                                { id_from = node_id
                                                , field = 1
                                                , id_to = old_id
                                                , time_from = right_time
                                                , time_to = current_time
                                                }]
                                            )
                                    
                                    -- Otherwise, the new right tree is a node, which have an id.
                                    TimeNode {t_id=new_id} ->
                                            if old_id == new_id
                                                then ( right_time, [] )
                                                
                                                -- If the id is different, then the edge must be frozen
                                                else ( current_time
                                                        , [TimeEdge
                                                            { id_from = node_id
                                                            , field = 1
                                                            , id_to = old_id
                                                            , time_from = right_time
                                                            , time_to = current_time
                                                            }]
                                                        )
                        in
                        
                        -- Return node at current position, with updated right tree.
                        -- The maximum element is the one from the recursive call.
                        -- The newly frozen edges are the recursive frozen edges, and possibly the right edge of this node.
                        ( TimeNode
                            { t_elm = node_elm
                            , t_id = node_id
                            , t_fields = [(left_time, left_tree), (new_right_time, new_right_tree)]
                            }
                        , max_maybe
                        , new_frozen ++ rec_frozen
                        )
    in

    -- Function to delete tree, and label time on edges
    -- Return tuple of new tree and new edges for the freezer
    -- TODO: refracter this shit
    let inner_delete tree
            = case tree of
                -- If the tree is a leaf, then the element does not exists, and it is therefore already deleted.
                TimeLeaf -> (TimeLeaf, [], [])

                -- If the tree is a node, then the tree paths must be checked
                TimeNode {t_elm=node_elm, t_id=node_id, t_fields=[(left_time, left_tree), (right_time, right_tree)]} ->
                    if e == node_elm
                        -- If the current element is equal to the element to be deleted, then the correct node is found.
                        then -- The current node is to be deleted, to both the left and right edge must be frozen.
                             -- Pattern match on the subtrees, to check if they exists.
                             let new_frozen =
                                    (case left_tree of
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
                             in
                             
                             -- The element to be placed at the current location is the predesessor, which must be the maximum element of the left tree.
                             let (new_left_tree, max_maybe, rec_frozen) = extract_max left_tree in
                             
                             -- Check if a max element was extracted
                             case max_maybe of
                                    -- If there is no max element, then the left tree is empty.
                                    -- The right tree can therefore be inserted at the current location of the tree.
                                    -- The frozen edges can only be the right edge of the current node.
                                    Nothing -> ( right_tree, new_frozen, [] )

                                    -- There is some max element, which is then inserted at the current location, with a new id.
                                    -- Frozen elements are the edges from the current node, and the frozen edges from the max extract.
                                    -- The id to element tuple is returned.
                                    Just max_elm ->
                                            ( TimeNode
                                                { t_elm = max_elm
                                                , t_id = next_id
                                                , t_fields = [(current_time, new_left_tree), (current_time, right_tree)]
                                                }
                                            , new_frozen ++ rec_frozen
                                            , [(next_id, max_elm)])
                        
                        -- Otherwise, check if the element must be in the left or right tree
                        else if e < node_elm
                                -- The element is smaller than the current element, and therefore must be in the left tree.
                                then -- Recursively update the left tree
                                     let (new_left_tree, rec_frozen, id_map) = inner_delete left_tree in
                                    
                                     -- If the node below in the left tree is updated, then the left edge must be frozen, and the time updated.
                                     -- Check if different using types of the old and new left tree, and the id's
                                     let (new_left_time, new_frozen) =
                                            case (left_tree, new_left_tree) of
                                                (TimeLeaf, TimeLeaf) ->
                                                        ( left_time, [] )
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
                                                            then ( left_time, [] )
                                                            else ( current_time
                                                                 , [TimeEdge
                                                                     { id_from = node_id
                                                                     , field = 0
                                                                     , id_to = old_id
                                                                     , time_from = left_time
                                                                     , time_to = current_time
                                                                     }]
                                                                 )
                                     
                                     -- Return the current node, with the updated left tree.
                                     -- The frozen edges are possible the left edge, and the frozen edges from the recursive call.
                                     in (TimeNode
                                            { t_elm = node_elm
                                            , t_id = node_id
                                            , t_fields = [(new_left_time, new_left_tree), (right_time, right_tree)]
                                            }
                                        , new_frozen ++ rec_frozen
                                        , id_map)
                                
                                -- Symmetric for recursion to the right.
                                else let (new_right_tree, rec_frozen, id_map) = inner_delete right_tree in
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
                                        , new_frozen ++ rec_frozen
                                        , id_map)
    in
    
    -- Construct new tree
    let old_tree = currentTree partialTree in
    let (new_tree, new_frozen, new_id_map) = inner_delete (old_tree) in
    
    -- Collect frozen edges
    let new_edgeFreezer = new_frozen ++ (edgeFreezer partialTree) in

    -- If root is deleted, then a new root appears
    let rootList_addition
            = case (old_tree, new_tree) of
                -- No root before => no root now
                (TimeLeaf, _) ->
                    []
                -- Root before, but no root now => new root
                (_, TimeLeaf) ->
                    [(current_time, -1)]
                -- If nodes before and after, check equality on the id, to see if there is a new root
                (TimeNode {t_id=old_node_id}, TimeNode {t_id=new_node_id}) ->
                    if old_node_id == new_node_id
                            then []
                            else [(current_time, new_node_id)]
    in
    let new_rootList = rootList_addition ++ (rootList partialTree) in

    -- Compute next id.
    -- At most one new node is created, and is so only if there is a new id_map element returned.
    let new_idCount
            = case new_id_map of
                [] -> next_id
                [(_, _)] -> next_id + 1
    in

    -- Return new partial tree structure, with updated tree, idToStatic list, rootmap and time.
    PartialTree
        { edgeFreezer = new_edgeFreezer
        , idStaticList = new_id_map ++ (idStaticList partialTree)
        , rootList = new_rootList
        , idCount = new_idCount
        , fieldCount = 2
        , time = current_time + 1
        , currentTree = new_tree
        }
