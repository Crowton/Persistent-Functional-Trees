{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}

module Persistent_update where

import DataRecords


construct_empty_tree :: Int -> PartialTree s
construct_empty_tree degree =
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

leaf :: Update s
leaf = id_func TimeLeaf

id_node :: UserTree s -> Update s
id_node UserLeaf = leaf
id_node (UserNode (x, con, _, fields)) = con x (map snd fields)

field_update :: Int -> State s -> [Update s] -> ([TimeTree s], State s)
field_update currentTime state fields =
    let (new_fields, new_state) =
            foldl (\(new_fields, state) field_func ->
                        let (new_field, new_state) = field_func (currentTime, state) in
                        (new_field : new_fields, new_state)
            ) ([], state) fields
    in
    (reverse new_fields, new_state)


create_new_node_from_edges :: s -> [TimeTree s] -> (Int, State s) -> (TimeTree s, State s)
create_new_node_from_edges elm fields (currentTime, (freezer, idMap, idCount)) =
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


create_new_node :: s -> [Update s] -> Update s
create_new_node elm fields (currentTime, state) =
    let (new_fields, field_state) = field_update currentTime state fields in
    create_new_node_from_edges elm new_fields (currentTime, field_state)


freeze_all_edges :: Int -> [(Int, TimeTree s)] -> (Int, State s) -> State s
freeze_all_edges old_id fields (currentTime, (freezer, idMap, idCount)) =
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
    let (new_fields, field_state) = field_update currentTime state new_fields_func in
    
    if old_elm == new_elm
        then
            let (field_freezer, field_idMap, field_idCount) = field_state in
            let (new_time_fields, new_freezer) =
                    foldl (\(new_time_fields, new_freezer) (field_num, (old_time, old_field), new_field) ->
                                let (field_time, freezer_update) =
                                        case (old_field, new_field) of
                                            (TimeLeaf, TimeNode {}) ->
                                                (currentTime, [])
                                            (TimeNode {t_id=old_field_id}, TimeLeaf) ->
                                                freeze old_field_id
                                            (TimeNode {t_id=old_field_id}, TimeNode {t_id=new_field_id}) | old_field_id /= new_field_id ->
                                                freeze old_field_id
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

        else
            let new_state = freeze_all_edges old_id old_fields (currentTime, field_state) in
            create_new_node_from_edges new_elm new_fields (currentTime, new_state)


replace_node_by_tree :: Int -> [(Int, TimeTree s)] -> Update s -> Update s
replace_node_by_tree old_id old_fields new_tree_func (currentTime, state) =
    let (new_tree, tree_state) = new_tree_func (currentTime, state) in
    let new_state = freeze_all_edges old_id old_fields (currentTime, tree_state) in

    ( new_tree
    , new_state
    )


create_user_tree :: Eq s => TimeTree s -> UserTree s
create_user_tree TimeLeaf = UserLeaf
create_user_tree TimeNode {t_id=id, t_elm=elm, t_fields=fields} =
    UserNode ( elm
             , replace_node_by_element id elm fields
             , replace_node_by_tree id fields
             , map (\(_, f) -> (create_user_tree f, id_func f)) fields
             )

tree_to_update :: Eq s => (UserTree s -> Update s) -> Update s -> Update s
tree_to_update func upd (currentTime, state) =
    let (tree, new_state) = upd (currentTime, state) in
    func (create_user_tree tree) (currentTime, new_state)


update :: Eq s => (t -> UserTree s -> Update s) -> t -> PartialTree s -> PartialTree s
update func val partialTree =
    let state_func = func val (create_user_tree (currentTree partialTree)) in
    let current_time = time partialTree in
    let (new_tree, (new_freezer, new_idMap, new_idCount)) = state_func (current_time, (edgeFreezer partialTree, idStaticList partialTree, idCount partialTree)) in

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

    PartialTree
        { edgeFreezer = new_freezer
        , idStaticList = new_idMap
        , rootList = new_rootList
        , idCount = new_idCount
        , fieldCount = fieldCount partialTree
        , time = current_time + 1
        , currentTree = new_tree
        }
