{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module PersistentUpdate where

import DataRecords


constructEmptyTree :: Int -> PartialTree s
constructEmptyTree degree =
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


idFunc :: t -> Update s t
idFunc val (_, state) = (val, state)

chainUpdate :: (t -> Update s u) -> Update s t -> Update s u
chainUpdate func upd (currentTime, state) =
    let (val, newState) = upd (currentTime, state) in
    func val (currentTime, newState)

-- Help function to return an update, that produce a leaf
leaf :: TreeUpdate s
leaf = idFunc TimeLeaf

-- Help function to make user defined functions work as identity functions
-- Can be used for some or all cases
idNode :: UserTree s -> TreeUpdate s
idNode UserLeaf = leaf
idNode (UserNode (x, con, _, fields)) = con x (map snd fields)

fieldUpdate :: Int -> State s -> [TreeUpdate s] -> ([TimeTree s], State s)
fieldUpdate currentTime state fields =
    -- Given a list of state functions for fields, produce a list of the field trees and the final state
    let (newFields, newState) =
            foldl (\(newFields, state) fieldFunc ->
                        let (newField, newState) = fieldFunc (currentTime, state) in
                        (newField : newFields, newState)
            ) ([], state) fields
    in
    (reverse newFields, newState)


createNewNodeFromEdges :: s -> [TimeTree s] -> (Int, State s) -> (TimeTree s, State s)
createNewNodeFromEdges elm fields (currentTime, (freezer, idMap, idCount)) =
    -- The next available id is the current idCount
    -- As a new node is created, all fields get the currentTime as start time
    -- No updates to the freezer, to allow calling the function from more contexts
    -- Update the idMap with the new id and static node value, and increase the idCount
    ( TimeNode
        { tElm = elm
        , tId = idCount
        , tFields = map (\f -> (currentTime, f)) fields
        }
    , ( freezer
      , (idCount, elm) : idMap
      , idCount + 1
      )
    )


-- Function to produce new node by the user
-- Must ONLY be called on UserLeaf cases!
createNewNode :: s -> [TreeUpdate s] -> TreeUpdate s
createNewNode elm fields (currentTime, state) =
    let (newFields, fieldState) = fieldUpdate currentTime state fields in
    createNewNodeFromEdges elm newFields (currentTime, fieldState)


freezeAllEdges :: Int -> [(Int, TimeTree s)] -> (Int, State s) -> State s
freezeAllEdges oldId fields (currentTime, (freezer, idMap, idCount)) =
    -- Update freezer to contain all edges to non leaf fields, with the end time being the currentTime
    ( foldl (\newFreezer (fieldNum, (oldTime, oldField)) ->
                (case oldField of
                    TimeNode {tId=oldFieldId} ->
                        [ TimeEdge
                            { idFrom = oldId
                            , field = fieldNum
                            , idTo = oldFieldId
                            , timeFrom = oldTime
                            , timeTo = currentTime
                            }
                        ]
                    _ -> []
                ) ++ newFreezer
      ) freezer (zip [0..] fields)
    , idMap
    , idCount
    )


replaceNodeByElement :: Eq s => Int -> s -> [(Int, TimeTree s)] -> s -> [TreeUpdate s] -> TreeUpdate s
replaceNodeByElement oldId oldElm oldFields newElm newFieldsFunc (currentTime, state) =
    -- Get list of fields and the state after production
    let (newFields, fieldState) = fieldUpdate currentTime state newFieldsFunc in
    
    if oldElm == newElm
        -- If the element in the node is the same, then the same node can be reused
        then
            -- Unfold the field state, to access the freezer
            let (fieldFreezer, fieldIdMap, fieldIdCount) = fieldState in
            
            -- Be comparing the old and new fields, the start time for each edge, and freezer updates can be determined
            let (newTimeFields, newFreezer) =
                    foldl (\(newTimeFields, newFreezer) (fieldNum, (oldTime, oldField), newField) ->
                                let (fieldTime, freezerUpdate) =
                                        case (oldField, newField) of
                                            -- A new edge is created, without an old one existing
                                            (TimeLeaf, TimeNode {}) ->
                                                (currentTime, [])
                                            -- An old edge stops existing, either from disapering entirely, or being replaced
                                            -- For both cases, the new edge gets a new start time, and the old edge is moved to the freezer
                                            (TimeNode {tId=oldFieldId}, TimeLeaf) ->
                                                freeze oldFieldId
                                            (TimeNode {tId=oldFieldId}, TimeNode {tId=newFieldId}) | oldFieldId /= newFieldId ->
                                                freeze oldFieldId
                                            -- Same edge, there is no new time and freezer updates
                                            _ -> (oldTime, [])
                                        where freeze oldFieldId =
                                                ( currentTime
                                                , [ TimeEdge
                                                    { idFrom = oldId
                                                    , field = fieldNum
                                                    , idTo = oldFieldId
                                                    , timeFrom = oldTime
                                                    , timeTo = currentTime
                                                    }
                                                ]
                                                )
                                in
                                ((fieldTime, newField) : newTimeFields, freezerUpdate ++ newFreezer)
                    ) ([], fieldFreezer) (zip3 [0..] oldFields newFields)
            in
            
            -- Return the old node, with new fields, and updates state
            ( TimeNode
                { tElm = oldElm
                , tId = oldId
                , tFields = reverse newTimeFields
                }
            , ( newFreezer
              , fieldIdMap
              , fieldIdCount
              )
            )

        -- The element in the node is new, and a new node is created
        else
            -- All old edges is moved to the freezer
            let newState = freezeAllEdges oldId oldFields (currentTime, fieldState) in
            -- A new node with new id is returned
            createNewNodeFromEdges newElm newFields (currentTime, newState)


replaceNodeByTree :: Int -> [(Int, TimeTree s)] -> TreeUpdate s -> TreeUpdate s
replaceNodeByTree oldId oldFields newTreeFunc (currentTime, state) =
    -- Get new tree and state from the tree function
    let (newTree, treeState) = newTreeFunc (currentTime, state) in
    
    -- The old node is replaced, so the edges of the node is moved to the freezer
    let newState = freezeAllEdges oldId oldFields (currentTime, treeState) in

    ( newTree
    , newState
    )


-- Recursive function to create UserTree from a TimeTree
createUserTree :: Eq s => TimeTree s -> UserTree s
createUserTree TimeLeaf = UserLeaf
createUserTree TimeNode {tId=id, tElm=elm, tFields=fields} =
    -- A UserNode needs the update functions for the user, with is supplied by the two functions above
    UserNode ( elm
             , replaceNodeByElement id elm fields
             , replaceNodeByTree id fields
             , map (\(_, f) -> (createUserTree f, idFunc f)) fields
             )


-- Help function, to chain user functions, which are (UserTree -> Update)
treeToUpdate :: Eq s => (UserTree s -> Update s t) -> TreeUpdate s -> Update s t
treeToUpdate func upd (currentTime, state) =
    -- Run the update parameter, the function needs, to get an intermediate state and TimeTree
    let (tree, newState) = upd (currentTime, state) in
    
    -- Convert the TimeTree back to UserTree, and call the user function with this parameter
    func (createUserTree tree) (currentTime, newState)


-- Decorator function for user functions
-- User function takes a value and UserTree input and produce an Update
-- This is used to perform the relevent updates to the persitent tree structure,
-- to make the function then work on PersistentTree
update :: Eq s => (t -> UserTree s -> TreeUpdate s) -> t -> PartialTree s -> PartialTree s
update func val partialTree =
    -- Use the user suplied function to make the state update function and tree production
    let stateFunc = func val (createUserTree (currentTree partialTree)) in

    -- Extra information needed is the current time
    let currentTime = time partialTree in

    -- The initial state is the current freezer, idStatic map and the idCount
    -- The final tree and state is uptained, using the state update function
    let (newTree, (newFreezer, newIdMap, newIdCount)) = stateFunc (currentTime, (edgeFreezer partialTree, idStaticList partialTree, idCount partialTree)) in

    -- Check if root of tree have altered, and update root list
    let newRootList =
            (case (currentTree partialTree, newTree) of
                (TimeLeaf, TimeNode {tId=newId}) ->
                    [(currentTime, newId)]
                (TimeNode {}, TimeLeaf) ->
                    [(currentTime, -1)]
                (TimeNode {tId=oldId}, TimeNode {tId=newId}) | oldId /= newId ->
                    [(currentTime, newId)]
                _ ->
                    []
            ) ++ (rootList partialTree)
    in
    
    -- Increase time, and return new tree, root list and state
    PartialTree
        { edgeFreezer = newFreezer
        , idStaticList = newIdMap
        , rootList = newRootList
        , idCount = newIdCount
        , fieldCount = fieldCount partialTree
        , time = currentTime + 1
        , currentTree = newTree
        }
