
module Query where

import DataRecords as D
import qualified Data.Map.Strict as MB
import qualified Data.List as L

-- TODO: forward parse optional information to recursive calls

query :: (Maybe s -> (Int -> r) -> r) -> MB.Map Int (FrozenNode s) -> Int -> r
query func rootMap time =
    let rootMaybe = MB.lookupLE time rootMap in
    let root = case rootMaybe of
            Just (_, root) -> root
            Nothing -> error "No root exists at time!"
    in
    
    let getField node fieldNum = case node of
            FrozenLeaf -> error "Leaf nodes do not have any fields!"
            FrozenInternalNode {fields=fields} ->
                let fieldList = fields L.!! fieldNum in
                let validEdges = filter (\e -> frozen_time_from e <= time && time < frozen_time_to e) fieldList in
                case validEdges of
                    [] -> traverse FrozenLeaf
                    [FrozenEdge {node_to=node_to}] -> traverse node_to
                    e -> error "Multiple valid edges at time!"

        traverse node =
            let info = case node of
                    FrozenLeaf -> Nothing
                    FrozenInternalNode {staticInformation=inf} -> Just inf
            in
            func info (getField node)
            
    in
    
    traverse root
