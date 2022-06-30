{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module DataRecords where


import GHC.Generics (Generic)
import Control.DeepSeq


-- Update structure

data TimeEdge = TimeEdge
    { idFrom :: Int
    , field :: Int
    , idTo :: Int
    , timeFrom :: Int
    , timeTo :: Int
    }
    deriving (Eq, Ord, Show, Generic, NFData)

data TimeTree s
    = TimeLeaf
    | TimeNode
        { tElm :: s
        , tId :: Int
        , tFields :: [(Int, TimeTree s)] -- (Exists from time, nodeto)
        }
    deriving (Eq, Show, Generic, NFData)

data PartialTree s = PartialTree
    { edgeFreezer :: [TimeEdge]
    , idStaticList :: [(Int, s)]  -- (id, staticInfo)
    , rootList :: [(Int, Int)]  -- (time, id), convention: id '-1' is the empty tree
    , idCount :: Int
    , fieldCount :: Int
    , time :: Int
    , currentTree :: TimeTree s
    }
    deriving (Eq, Show, Generic, NFData)


type EPH_BST t s =
    ( Tree s                 -- empty function
    , t -> Tree s -> Tree s  -- insert function
    , t -> Tree s -> Tree s  -- delete function
    )

type PER_BST t s =
    ( PartialTree s                        -- empty function
    , t -> PartialTree s -> PartialTree s  -- insert function
    , t -> PartialTree s -> PartialTree s  -- delete function
    )


-- User viewed update structure

type State s = ([TimeEdge], [(Int, s)], Int)
type Update s t = (Int, State s) -> (t, State s)
type TreeUpdate s = Update s (TimeTree s)

data UserTree s
    = UserLeaf
    | UserNode (s, s -> [TreeUpdate s] -> TreeUpdate s, TreeUpdate s -> TreeUpdate s, [(UserTree s, TreeUpdate s)])
    -- Input: element in the node, func to make node from element and fields, func to replace node by build tree, list of fields


-- Nodes used in DAG

data FrozenNode staticType = FrozenNode
    { staticInformation :: staticType
    , fields :: [FrozenEdge staticType]
    }
    deriving (Show, Generic, NFData)

data FrozenEdge staticType = FrozenEdge
    { fieldFrom :: Int
    , nodeTo :: FrozenNode staticType
    , frozenTimeFrom :: Int
    , frozenTimeTo :: Int
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
