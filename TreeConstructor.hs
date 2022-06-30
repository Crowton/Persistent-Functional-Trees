
module TreeConstructor where

import DataRecords as D

import qualified Data.List as L
import qualified Data.Map.Strict as MB


construct ::  Int -> MB.Map Int (Maybe (FrozenNode s)) -> Int -> Tree s
construct fieldCount rootMap time =
    let root = case MB.lookupLE time rootMap of
            Nothing -> error ("No root at time " ++ show time ++ "!")
            Just (_, root) -> root
    in

    let gen frozenTree
            = case frozenTree of
                Nothing -> Leaf
                Just FrozenNode {staticInformation=staticInformation, fields=fields} ->
                    let children
                            = map (\fieldNum ->
                                    let validEdges
                                            = filter (\e ->
                                                        fieldFrom e == fieldNum
                                                        && frozenTimeFrom e <= time
                                                        && time < frozenTimeTo e
                                            ) fields in
                                    case validEdges of
                                        [] -> gen Nothing
                                        [FrozenEdge {nodeTo=nodeTo}] ->
                                            gen (Just nodeTo)
                                        _ -> error ("Multiple valid edges at time " ++ show time ++ "!")
                            ) [0 .. (fieldCount - 1)]
                    in
                    Node {
                        elm = staticInformation,
                        children = children
                    }
    in

    gen root
