
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

data TimeTree s
    = TimeLeaf
    | TimeNode
        { t_elm :: s
        , t_id :: Int
        , t_fields :: [(Int, TimeTree s)] -- (Exists from time, nodeto)
        }
    deriving (Show)

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
    deriving (Show)


-- Nodes used in DAG

data FrozenNode staticType = FrozenNode
    { staticInformation :: staticType
    , fields :: [FrozenEdge staticType]
    }
    deriving (Show)

data FrozenEdge staticType = FrozenEdge
    { field_from :: Int
    , node_to :: FrozenNode staticType
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
    deriving (Eq, Show)
