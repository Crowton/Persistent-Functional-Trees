
module Construct where

import DataRecords as D

import qualified Data.List as L
import qualified Data.Map.Strict as MB

-- construct :: MB.Map Int (FrozenNode s) -> Int -> Tree s
-- construct rootMap time =
--     let root = case MB.lookupLE time rootMap of
--             Just (_, root) -> root
--             Nothing -> error ("No root exists at time " ++ (show time) ++ "!")
--     in
    
--     let inner frozenTree
--             = case frozenTree of
--                 FrozenLeaf -> Leaf

