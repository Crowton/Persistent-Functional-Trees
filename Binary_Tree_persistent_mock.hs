module Binary_Tree_persistent_mock where

import DataRecords

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
                TimeNode {t_elm=t_elm, t_id=t_id, t_fields=[(left_time, left_tree), (right_time, right_tree)]} ->
                    if e == t_elm
                        then tree
                        else if e < t_elm
                                then let new_left_time = case left_tree of
                                            TimeLeaf -> current_time
                                            TimeNode {} -> left_time
                                     in TimeNode
                                            { t_elm = t_elm
                                            , t_id = t_id
                                            , t_fields = [(new_left_time, inner_insert left_tree), (right_time, right_tree)]
                                            }
                                else let new_right_time = case right_tree of
                                            TimeLeaf -> current_time
                                            TimeNode {} -> right_time
                                     in TimeNode
                                            { t_elm = t_elm
                                            , t_id = t_id
                                            , t_fields = [(left_time, left_tree), (new_right_time, inner_insert right_tree)]
                                            }
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
    
    PartialTree
        { edgeFreezer = edgeFreezer partialTree
        , idStaticList = (next_id, e) : (idStaticList partialTree)
        , rootList = new_rootList
        , idCount = next_id + 1
        , fieldCount = 2
        , time = current_time + 1
        , currentTree = new_tree
        }
