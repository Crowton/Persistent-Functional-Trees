{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Persistent_update where

import DataRecords
import Data.ByteString (partition)


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
id_func node (currentTime, (freezer, idMap, idCount)) = (node, (freezer, idMap, idCount))

leaf :: Update s
leaf = id_func TimeLeaf 

field_update :: Int -> State s -> [Update s] -> ([(Int, TimeTree s)], State s)
field_update currentTime (freezer, idMap, idCount) fields =
    let (new_fields, new_state) =
            foldl (\(new_fields, state) field_func ->
                        let (new_field, new_state) = field_func (currentTime, state) in
                        (new_field : new_fields, new_state)
            ) ([], (freezer, idMap, idCount)) fields
    in
    let new_time_fields = map (\f -> (currentTime, f)) (reverse new_fields) in
    (new_time_fields, new_state)

create_new_node :: s -> [Update s] -> Update s
create_new_node staticVal fields (currentTime, state) =
    let (new_time_fields, (field_freezer, field_idMap, field_idCount)) =
            field_update currentTime state fields in
    
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
update_node_fields old_id old_elm old_fields new_fields (currentTime, state) =
    let (new_time_fields, (field_freezer, field_idMap, field_idCount)) =
            field_update currentTime state new_fields in

    let new_freezer = 
            foldl (\new_freezer (field_num, (old_time, old_field), (_, new_field)) ->
                        (case (old_field, new_field) of
                            (TimeNode {t_id=old_field_id}, TimeNode {t_id=new_field_id}) | old_field_id /= new_field_id ->
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
            ) field_freezer (zip3 [0..] old_fields new_time_fields)
    in

    ( TimeNode
        { t_elm = old_elm
        , t_id = old_id
        , t_fields = new_time_fields
        }
    , ( new_freezer
      , field_idMap
      , field_idCount
      )
    )


replace_node :: Int -> [(Int, TimeTree s)] -> s -> [Update s] -> Update s
replace_node old_id old_fields staticVal new_fields (currentTime, state) =
    let (new_time_fields, (field_freezer, field_idMap, field_idCount)) =
            field_update currentTime state new_fields in
    
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

create_user_tree :: TimeTree s -> UserTree s
create_user_tree TimeLeaf = UserLeaf
create_user_tree TimeNode {t_id=id, t_elm=elm, t_fields=fields} =
    UserNode ( elm
             , update_node_fields id elm fields
             , replace_node id fields
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
