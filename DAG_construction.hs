
module DAG_construction where

import DataRecords as D
import Tree_Constructor as T

import Data.List
import qualified Data.HashMap.Strict as MH
import qualified Data.Map.Strict as MB

import Debug.Trace
import Language.Haskell.TH

-- Fields are integers in the range [0 .. d-1]

build :: Show s => PartialTree s -> (Int -> Tree s)
build partialTree =
    -- Extract all edges in current tree
    let finish_time = time partialTree in
    let treeEdgeExtract tree edgeList = case tree of
            TimeLeaf -> edgeList
            TimeNode {t_id=t_id_from, t_fields=t_fields} ->
                fst (foldl (\(edgeList, field_num) (time_from, child) ->
                            ( treeEdgeExtract
                                child
                                ((case child of
                                    TimeLeaf -> []
                                    TimeNode {t_id=t_id_to} ->
                                        [ TimeEdge
                                            { id_from = t_id_from
                                            , field = field_num
                                            , id_to = t_id_to
                                            , time_from = time_from
                                            , time_to = finish_time
                                            }
                                        ]
                                 )
                                ++ edgeList)
                            , field_num + 1)
                ) (edgeList, 0) t_fields)
    in

    let edgeList = treeEdgeExtract (currentTree partialTree) (edgeFreezer partialTree) in      

    -- Map from id to static information
    let idToStatic = MH.fromList (idStaticList partialTree) in

    -- Forwards and Bcakwards incidense lists
    let forwards
            = foldl (\m e ->
                    let edges = MH.findWithDefault [] (id_from e) m in
                    MH.insert (id_from e) (e : edges) m
            ) MH.empty edgeList in
    let backwards
            = foldl (\m e ->
                let edges = MH.findWithDefault [] (id_to e) m in
                MH.insert (id_to e) (id_from e : edges) m
            ) MH.empty edgeList in
    
    -- Current out degree of each node
    let outDegree = MH.map length forwards in
    
    -- Nodes with zero out degree
    let zeroOutDegree = filter (\id -> not (MH.member id outDegree)) [0 .. (idCount partialTree) - 1] in

    -- Function to create DAG, by processing nodes of zero out degree
    let innerRec zeroOutDegree outDegree idToInstance = case zeroOutDegree of
            [] -> idToInstance
            id : ids -> 
                let fields
                        = map (\e ->
                            FrozenEdge 
                                { field_from = field e
                                , node_to = idToInstance MH.! (id_to e)
                                , frozen_time_from = time_from e
                                , frozen_time_to = time_to e
                                }
                        ) (MH.findWithDefault [] id forwards) in
                let node = FrozenNode {staticInformation = idToStatic MH.! id, fields = fields} in
                let parents = MH.findWithDefault [] id backwards in
                let newOutDegree = foldl (\m p -> MH.insert p (m MH.! p - 1) m) outDegree parents in
                let newZeroOut = filter (\p -> newOutDegree MH.! p == 0) parents in
                innerRec (newZeroOut ++ ids) newOutDegree (MH.insert id node idToInstance)
    in
    
    -- Create id to instance
    let idToNode = innerRec zeroOutDegree outDegree MH.empty in
    
    -- Create rootmap
    let newRootList = (finish_time, -1) : (rootList partialTree) in
    let rootNodeList = map (\(t, r) -> (t, if r == -1 then Nothing else Just (idToNode MH.! r))) newRootList in
    
    -- TODO: kill root node at finish time OR make actual leafs
    -- Create root map
    let rootMap = MB.fromDistinctDescList rootNodeList in
    
    -- Return tree constructor
    T.construct (fieldCount partialTree) rootMap
