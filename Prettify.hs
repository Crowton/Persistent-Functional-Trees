{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module Prettify where

import Data.String
import Data.List

import DataRecords


prettyTree :: Show e => Tree e -> String
prettyTree tree =
    let toStr tree indent = case tree of
            Leaf -> indent ++ "|- "
            Node {elm=elm, children=children} ->
                indent ++ "|- " ++ (show elm) ++ "\n" ++ (intercalate "\n" (map (\c -> toStr c (indent ++ "|  ")) children))
    in
    toStr tree ""


prettyTimeTree :: Show e => Int -> TimeTree e -> String
prettyTimeTree time tree =
    let toStr tree time indent = case tree of
            TimeLeaf -> indent ++ "|- (" ++ (show time) ++ ")"
            TimeNode {tElm=elm, tFields=children} ->
                indent ++ "|- " ++ (show elm) ++ " (" ++ (show time) ++ ") \n" ++ (intercalate "\n" (map (\(t, c) -> toStr c t (indent ++ "|  ")) children))
    in
    toStr tree time ""
