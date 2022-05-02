{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Persistent_update where

import DataRecords


construct_empty_tree :: Int -> PartialTree s
construct_empty_tree degree =
    -- Only the fieldCount / degree of the tree is relevant to construct a new empty partial tree
    PartialTree {
        edgeFreezer = [],
        idStaticList = [],
        rootList = [(0, -1)],
        idCount = 0,
        fieldCount = degree,
        time = 1,
        currentTree = TimeLeaf
    }


id_func :: TimeTree s -> Update s
id_func node (_, state) = (node, state)

-- Help function to return an update, that produce a leaf
leaf :: Update s
leaf = id_func TimeLeaf

-- Help function to make user defined functions work as identity functions
-- Can be used for some or all cases
id_node :: UserTree s -> Update s
id_node UserLeaf = leaf
id_node (UserNode (x, con, _, fields)) = con x (map snd fields)

field_update :: Int -> State s -> [Update s] -> ([TimeTree s], State s)
field_update currentTime state fields =
    -- Given a list of state functions for fields, produce a list of the field trees and the final state
    let (new_fields, new_state) =
            foldl (\(new_fields, state) field_func ->
                        let (new_field, new_state) = field_func (currentTime, state) in
                        (new_field : new_fields, new_state)
            ) ([], state) fields
    in
    (reverse new_fields, new_state)


create_new_node_from_edges :: s -> [TimeTree s] -> (Int, State s) -> (TimeTree s, State s)
create_new_node_from_edges elm fields (currentTime, (freezer, idMap, idCount)) =
    -- The next available id is the current idCount
    -- As a new node is created, all fields get the currentTime as start time
    -- No updates to the freezer, to allow calling the function from more contexts
    -- Update the idMap with the new id and static node value, and increase the idCount
    ( TimeNode
        { t_elm = elm
        , t_id = idCount
        , t_fields = map (\f -> (currentTime, f)) fields
        }
    , ( freezer
      , (idCount, elm) : idMap
      , idCount + 1
      )
    )


-- Function to produce new node by the user
-- Must ONLY be called on UserLeaf cases!
create_new_node :: s -> [Update s] -> Update s
create_new_node elm fields (currentTime, state) =
    let (new_fields, field_state) = field_update currentTime state fields in
    create_new_node_from_edges elm new_fields (currentTime, field_state)


freeze_all_edges :: Int -> [(Int, TimeTree s)] -> (Int, State s) -> State s
freeze_all_edges old_id fields (currentTime, (freezer, idMap, idCount)) =
    -- Update freezer to contain all edges to non leaf fields, with the end time being the currentTime
    ( foldl (\new_freezer (field_num, (old_time, old_field)) ->
                (case old_field of
                    TimeNode {t_id=old_field_id} ->
                        [ TimeEdge
                            { id_from = old_id
                            , field = field_num
                            , id_to = old_field_id
                            , time_from = old_time
                            , time_to = currentTime
                            }
                        ]
                    _ -> []
                ) ++ new_freezer
      ) freezer (zip [0..] fields)
    , idMap
    , idCount
    )


replace_node_by_element :: Eq s => Int -> s -> [(Int, TimeTree s)] -> s -> [Update s] -> Update s
replace_node_by_element old_id old_elm old_fields new_elm new_fields_func (currentTime, state) =
    -- Get list of fields and the state after production
    let (new_fields, field_state) = field_update currentTime state new_fields_func in
    
    if old_elm == new_elm
        -- If the element in the node is the same, then the same node can be reused
        then
            -- Unfold the field state, to access the freezer
            let (field_freezer, field_idMap, field_idCount) = field_state in
            
            -- Be comparing the old and new fields, the start time for each edge, and freezer updates can be determined
            let (new_time_fields, new_freezer) =
                    foldl (\(new_time_fields, new_freezer) (field_num, (old_time, old_field), new_field) ->
                                let (field_time, freezer_update) =
                                        case (old_field, new_field) of
                                            -- A new edge is created, without an old one existing
                                            (TimeLeaf, TimeNode {}) ->
                                                (currentTime, [])
                                            -- An old edge stops existing, either from disapering entirely, or being replaced
                                            -- For both cases, the new edge gets a new start time, and the old edge is moved to the freezer
                                            (TimeNode {t_id=old_field_id}, TimeLeaf) ->
                                                freeze old_field_id
                                            (TimeNode {t_id=old_field_id}, TimeNode {t_id=new_field_id}) | old_field_id /= new_field_id ->
                                                freeze old_field_id
                                            -- Same edge, there is no new time and freezer updates
                                            _ -> (old_time, [])
                                        where freeze old_field_id =
                                                ( currentTime
                                                , [ TimeEdge
                                                    { id_from = old_id
                                                    , field = field_num
                                                    , id_to = old_field_id
                                                    , time_from = old_time
                                                    , time_to = currentTime
                                                    }
                                                ]
                                                )
                                in
                                ((field_time, new_field) : new_time_fields, freezer_update ++ new_freezer)
                    ) ([], field_freezer) (zip3 [0..] old_fields new_fields)
            in
            
            -- Return the old node, with new fields, and updates state
            ( TimeNode
                { t_elm = old_elm
                , t_id = old_id
                , t_fields = reverse new_time_fields
                }
            , ( new_freezer
              , field_idMap
              , field_idCount
              )
            )

        -- The element in the node is new, and a new node is created
        else
            -- All old edges is moved to the freezer
            let new_state = freeze_all_edges old_id old_fields (currentTime, field_state) in
            -- A new node with new id is returned
            create_new_node_from_edges new_elm new_fields (currentTime, new_state)


replace_node_by_tree :: Int -> [(Int, TimeTree s)] -> Update s -> Update s
replace_node_by_tree old_id old_fields new_tree_func (currentTime, state) =
    -- Get new tree and state from the tree function
    let (new_tree, tree_state) = new_tree_func (currentTime, state) in
    
    -- The old node is replaced, so the edges of the node is moved to the freezer
    let new_state = freeze_all_edges old_id old_fields (currentTime, tree_state) in

    ( new_tree
    , new_state
    )


-- Recursive function to create UserTree from a TimeTree
create_user_tree :: Eq s => TimeTree s -> UserTree s
create_user_tree TimeLeaf = UserLeaf
create_user_tree TimeNode {t_id=id, t_elm=elm, t_fields=fields} =
    -- A UserNode needs the update functions for the user, with is supplied by the two functions above
    UserNode ( elm
             , replace_node_by_element id elm fields
             , replace_node_by_tree id fields
             , map (\(_, f) -> (create_user_tree f, id_func f)) fields
             )


-- Help function, to chain user functions, which are (UserTree -> Update)
tree_to_update :: Eq s => (UserTree s -> Update s) -> Update s -> Update s
tree_to_update func upd (currentTime, state) =
    -- Run the update parameter, the function needs, to get an intermediate state and TimeTree
    let (tree, new_state) = upd (currentTime, state) in
    
    -- Convert the TimeTree back to UserTree, and call the user function with this parameter
    func (create_user_tree tree) (currentTime, new_state)


-- Decorator function for user functions
-- User function takes a value and UserTree input and produce an Update
-- This is used to perform the relevent updates to the persitent tree structure,
-- to make the function then work on PersistentTree
update :: Eq s => (t -> UserTree s -> Update s) -> t -> PartialTree s -> PartialTree s
update func val partialTree =
    -- Use the user suplied function to make the state update function and tree production
    let state_func = func val (create_user_tree (currentTree partialTree)) in

    -- Extra information needed is the current time
    let current_time = time partialTree in

    -- The initial state is the current freezer, idStatic map and the idCount
    -- The final tree and state is uptained, using the state update function
    let (new_tree, (new_freezer, new_idMap, new_idCount)) = state_func (current_time, (edgeFreezer partialTree, idStaticList partialTree, idCount partialTree)) in

    -- Check if root of tree have altered, and update root list
    let new_rootList =
            (case (currentTree partialTree, new_tree) of
                (TimeLeaf, TimeNode {t_id=new_id}) ->
                    [(current_time, new_id)]
                (TimeNode {}, TimeLeaf) ->
                    [(current_time, -1)]
                (TimeNode {t_id=old_id}, TimeNode {t_id=new_id}) | old_id /= new_id ->
                    [(current_time, new_id)]
                _ ->
                    []
            ) ++ (rootList partialTree)
    in
    
    -- Increase time, and return new tree, root list and state
    PartialTree
        { edgeFreezer = new_freezer
        , idStaticList = new_idMap
        , rootList = new_rootList
        , idCount = new_idCount
        , fieldCount = fieldCount partialTree
        , time = current_time + 1
        , currentTree = new_tree
        }
