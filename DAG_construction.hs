
module DAG_construction where

import DataRecords as D
import Tree_Constructor as T

import Data.List
import qualified Data.HashMap.Strict as MH
import qualified Data.Map.Strict as MB

import Debug.Trace
import Language.Haskell.TH

-- Fields are integers in the range [0 .. d-1]

build :: PartialTree s -> (Int -> Tree s)
build partialTree =
    let edgeList = edgeFreezer partialTree in
    
    let idToStatic = MH.fromList (idStaticList partialTree) in

    let forwards
            = map ( \i ->
                let relevantEdges = filter (\e -> field e == i) edgeList in
                foldl (\m e ->
                    let edges = MH.findWithDefault [] (id_from e) m in
                    MH.insert (id_from e) (e : edges) m
                ) MH.empty relevantEdges
            ) [0 .. (fieldCount partialTree) - 1] in

    let backwards
            = foldl (\m e ->
                let edges = MH.findWithDefault [] (id_to e) m in
                MH.insert (id_to e) (id_from e : edges) m
            ) MH.empty edgeList in
    
    let outDegree
            = foldl (\m e ->
                let degree = MH.findWithDefault 0 (id_from e) m in
                MH.insert (id_from e) (degree + 1) m
            ) MH.empty edgeList in
    
    let zeroOutDegree = filter (\id -> not (MH.member id outDegree)) [0 .. (idCount partialTree) - 1] in

    let innerRec zeroOutDegree outDegree idToInstance = case zeroOutDegree of
            [] -> idToInstance
            id : ids -> 
                -- trace ("Processing... " ++ (show id)) $
                let fields
                        = map (\m ->
                            let edges = MH.findWithDefault [] id m in
                            map (\e -> FrozenEdge 
                                        { node_to = idToInstance MH.! (id_to e)
                                        , frozen_time_from = time_from e
                                        , frozen_time_to = time_to e
                                        }
                                ) edges
                        ) forwards in
                let node = FrozenNode {staticInformation = idToStatic MH.! id, fields = fields} in
                let parents = MH.findWithDefault [] id backwards in
                let newOutDegree = foldl (\m p -> MH.insert p (m MH.! p - 1) m) outDegree parents in
                let newZeroOut = filter (\p -> newOutDegree MH.! p == 0) parents in
                innerRec (newZeroOut ++ ids) newOutDegree (MH.insert id node idToInstance)
    in
    
    let idToNode = innerRec zeroOutDegree outDegree MH.empty in
    let rootNodeList = map (\(t, r) -> (t, idToNode MH.! r)) (rootList partialTree) in
    let rootMap = MB.fromDistinctAscList rootNodeList in  -- TODO: decending list, as roots are added first to the list!
    
    T.construct rootMap

    -- lookupLE


-- main = print ((build [
--         Edge {id_from = 0, field = 0, id_to = 1, time_from = 0, time_to = 10},
--         Edge {id_from = 0, field = 1, id_to = 2, time_from = 0, time_to = 4}
--     ] [(0, 0 :: Int), (1, 1 :: Int), (2, 2 :: Int)] 3 2) M.! 0)
