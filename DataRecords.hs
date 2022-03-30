
module DataRecords where


-- Update structure

data TimeEdge = TimeEdge
    { id_from :: Int
    , field :: Int
    , id_to :: Int
    , time_from :: Int
    , time_to :: Int
    }
    deriving (Show)

data PartialTree s = PartialTree
    { edgeFreezer :: [TimeEdge]
    , idStaticList :: [(Int, s)]
    , rootList :: [(Int, Int)]
    , idCount :: Int
    , fieldCount :: Int
    , time :: Int
    , currentTree :: Tree s
    }


-- Nodes used in DAG

data FrozenNode staticType = FrozenNode
    { staticInformation :: staticType
    , fields :: [[FrozenEdge staticType]]
    }
    deriving (Show)

data FrozenEdge staticType = FrozenEdge
    { node_to :: FrozenNode staticType
    , frozen_time_from :: Int
    , frozen_time_to :: Int
    }
    deriving (Show)


-- Nodes exposed to user under update and query

data Tree t
    = Leaf
    | Node
        { elm :: t
        , children :: [Tree t]
        }
