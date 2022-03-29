
module Query where

import DataRecords as D
import qualified Data.Map.Strict as MB
import qualified Data.List as L

-- TODO: forward parse optional information to recursive calls

query :: (Maybe s -> (Int -> r) -> r) -> MB.Map Int (FrozenNode s) -> Int -> r
query func rootMap time =
    let root = case MB.lookupLE time rootMap of
            Nothing -> Nothing
            Just (_, root) -> Just root
    in
    
    let getField node fieldNum = case node of
            Nothing -> error "Leaf nodes do not have any fields!"
            Just (FrozenNode {fields=fields}) ->
                let fieldList = fields L.!! fieldNum in
                let validEdges = filter (\e -> frozen_time_from e <= time && time < frozen_time_to e) fieldList in
                case validEdges of
                    [] -> traverse Nothing
                    [FrozenEdge {node_to=node_to}] -> traverse (Just node_to)
                    e -> error "Multiple valid edges at time!"

        traverse node =
            let info = case node of
                    Nothing -> Nothing
                    Just (FrozenNode {staticInformation=inf}) -> Just inf
            in
            func info (getField node)
            
    in
    
    traverse root
