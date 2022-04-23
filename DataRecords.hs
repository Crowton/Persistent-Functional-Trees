{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module DataRecords where


import GHC.Generics (Generic)
import Control.DeepSeq


-- Update structure

data TimeEdge = TimeEdge
    { id_from :: Int
    , field :: Int
    , id_to :: Int
    , time_from :: Int
    , time_to :: Int
    }
    deriving (Eq, Show, Generic, NFData)

data TimeTree s
    = TimeLeaf
    | TimeNode
        { t_elm :: s
        , t_id :: Int
        , t_fields :: [(Int, TimeTree s)] -- (Exists from time, nodeto)
        }
    deriving (Eq, Show, Generic, NFData)

-- User viewed update structure

type State s = ([TimeEdge], [(Int, s)], Int)
type Update s = (Int, State s) -> (TimeTree s, State s)

data UserTree s
    = UserLeaf
    | UserNode (s, [Update s] -> Update s, s -> [Update s] -> Update s, [(UserTree s, Update s)])

-- TODO: add dynamic information
data PartialTree s = PartialTree
    { edgeFreezer :: [TimeEdge]
    , idStaticList :: [(Int, s)]  -- (id, static_info)
    , rootList :: [(Int, Int)]  -- (time, id), convention: id '-1' is the empty tree
    , idCount :: Int
    , fieldCount :: Int
    , time :: Int
    , currentTree :: TimeTree s
    }
    deriving (Eq, Show, Generic, NFData)


-- Nodes used in DAG

data FrozenNode staticType = FrozenNode
    { staticInformation :: staticType
    , fields :: [FrozenEdge staticType]
    }
    deriving (Show, Generic, NFData)

data FrozenEdge staticType = FrozenEdge
    { field_from :: Int
    , node_to :: FrozenNode staticType
    , frozen_time_from :: Int
    , frozen_time_to :: Int
    }
    deriving (Show, Generic, NFData)


-- Nodes exposed to user under query

data Tree t
    = Leaf
    | Node
        { elm :: t
        , children :: [Tree t]
        }
    deriving (Eq, Show, Generic, NFData)
