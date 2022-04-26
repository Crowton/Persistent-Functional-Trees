{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}

module Persistent_update where

import DataRecords
import Data.Either
import Data.Coerce (coerce, Coercible)
import Data.Function


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

field_update :: Int -> State s -> [Update s] -> ([TimeTree s], State s)
field_update currentTime state fields =
    let (new_fields, new_state) =
            foldl (\(new_fields, state) field_func ->
                        let (new_field, new_state) = field_func (currentTime, state) in
                        (new_field : new_fields, new_state)
            ) ([], state) fields
    in
    (reverse new_fields, new_state)

create_new_node :: s -> [Update s] -> Update s
create_new_node staticVal fields (currentTime, state) =
    let (new_fields, (field_freezer, field_idMap, field_idCount)) =
            field_update currentTime state fields in
    let new_time_fields = map (\f -> (currentTime, f)) new_fields in

    ( TimeNode
        { t_elm = staticVal
        , t_id = field_idCount
        , t_fields = new_time_fields
        }
    , ( field_freezer
      , (field_idCount, staticVal) : field_idMap
      , field_idCount + 1
      )
    )


update_node_fields :: Int -> s -> [(Int, TimeTree s)] -> [Update s] -> Update s
update_node_fields old_id old_elm old_fields new_fields_func (currentTime, state) =
    let (new_fields, (field_freezer, field_idMap, field_idCount)) =
            field_update currentTime state new_fields_func in

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


replace_node_by_element :: Int -> [(Int, TimeTree s)] -> s -> [Update s] -> Update s
replace_node_by_element old_id old_fields staticVal new_fields_func (currentTime, state) =
    let (new_fields, (field_freezer, field_idMap, field_idCount)) =
            field_update currentTime state new_fields_func in
    let new_time_fields = map (\f -> (currentTime, f)) new_fields in

    let new_freezer =
            foldl (\new_freezer (field_num, (old_time, old_field)) ->
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
            ) field_freezer (zip [0..] old_fields)
    in

    ( TimeNode
        { t_elm = staticVal
        , t_id = field_idCount
        , t_fields = new_time_fields
        }
    , ( new_freezer
      , (field_idCount, staticVal) : field_idMap
      , field_idCount + 1
      )
    )


replace_node_by_tree :: Int -> [(Int, TimeTree s)] -> Update s -> Update s
replace_node_by_tree old_id old_fields new_tree_func (currentTime, state) =
    let (new_tree, (new_freezer, new_idMap, new_idCount)) = new_tree_func (currentTime, state) in

    let new_new_freezer =
            foldl (\new_freezer (field_num, (old_time, old_field)) ->
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
            ) new_freezer (zip [0..] old_fields)
    in

    ( new_tree
    , ( new_new_freezer
      , new_idMap
      , new_idCount
      )
    )



convert :: ValuesToParameter a s => a -> FunctionParameters s
convert new_values = toParam new_values

-- construct_node_from_node :: Coercible a (FunctionParameters s) => Int -> s -> [(Int, TimeTree s)] -> a -> Update s
-- construct_node_from_node :: ValuesToParameter a s => Int -> s -> [(Int, TimeTree s)] -> (a -> Update s)
construct_node_from_node :: Int -> s -> [(Int, TimeTree s)] -> FunctionParameters s -> Update s
-- construct_node_from_node :: ValuesToParameter a s => Int -> s -> [(Int, TimeTree s)] -> (ValuesToParameter a s => a) -> Update s
construct_node_from_node old_id old_elm old_fields new_values (currentTime, state) =
    -- case coerce new_values of
    case new_values of
        ParameterNode (new_elm, new_fields_func) ->
                let (new_fields, (field_freezer, field_idMap, field_idCount)) =
                        field_update currentTime state new_fields_func in
                let new_time_fields = map (\f -> (currentTime, f)) new_fields in

                let new_freezer =
                        foldl (\new_freezer (field_num, (old_time, old_field)) ->
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
                        ) field_freezer (zip [0..] old_fields)
                in

                ( TimeNode
                    { t_elm = new_elm
                    , t_id = field_idCount
                    , t_fields = new_time_fields
                    }
                , ( new_freezer
                  , (field_idCount, new_elm) : field_idMap
                  , field_idCount + 1
                  )
                )

        ParameterTree new_tree_func ->
            let (new_tree, (new_freezer, new_idMap, new_idCount)) = new_tree_func (currentTime, state) in

            let new_new_freezer =
                    foldl (\new_freezer (field_num, (old_time, old_field)) ->
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
                    ) new_freezer (zip [0..] old_fields)
            in

            ( new_tree
            , ( new_new_freezer
              , new_idMap
              , new_idCount
              )
            )


create_user_tree :: TimeTree s -> UserTree s
create_user_tree TimeLeaf = UserLeaf
create_user_tree TimeNode {t_id=id, t_elm=elm, t_fields=fields} =
    UserNode ( elm
             , convert & construct_node_from_node id elm fields
             , map (\(_, f) -> (create_user_tree f, id_func f)) fields
             )

update :: (t -> UserTree s -> Update s) -> t -> PartialTree s -> PartialTree s
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
