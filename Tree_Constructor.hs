
module Tree_Constructor where

import DataRecords as D

import qualified Data.List as L
import qualified Data.Map.Strict as MB

import Debug.Trace

construct :: MB.Map Int (FrozenNode s) -> Int -> Tree s
construct rootMap time =
    let root = case MB.lookupLE time rootMap of
            Nothing -> Nothing
            Just (_, root) -> Just root
    in
    
    let gen frozenTree
            = case frozenTree of
                Nothing -> Leaf
                Just (FrozenNode {staticInformation=staticInformation, fields=fields}) ->
                    -- trace ("Generating node " ++ (show staticInformation) ++ " ...") $
                    let children
                            = map (\edgeList ->
                                    let validEdges = filter (\e -> frozen_time_from e <= time && time < frozen_time_to e) edgeList in
                                    case validEdges of
                                        [] -> gen Nothing
                                        [FrozenEdge {node_to=node_to}] -> gen (Just node_to)
                                        _ -> error "Multiple valid edges at time!"
                            ) fields
                    in
                    Node {
                        elm = staticInformation,
                        children = children
                    }
    in
    
    gen root
