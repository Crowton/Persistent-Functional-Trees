
module DataRecords where

data TimeEdge = TimeEdge
    { id_from :: Int
    , field :: Int
    , id_to :: Int
    , time_from :: Int
    , time_to :: Int
    }
    deriving (Show)


-- Nodes used in DAG

data FrozenEdge staticType = FrozenEdge
    { node_to :: FrozenNode staticType
    , frozen_time_from :: Int
    , frozen_time_to :: Int
    }
    deriving (Show)

data FrozenTree staticType
    = FrozenLeaf
    | FrozenInternalNode
        { staticInformation :: staticType
        , fields :: [[FrozenEdge staticType]]
        }
    deriving (Show)


-- Nodes exposed to user under update and query

data Tree t
    = Leaf
    | Node
        { elm :: t
        , children :: [Tree t]
        }
