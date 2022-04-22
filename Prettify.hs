{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Prettify where

import Data.String
import Data.List

import DataRecords

import Debug.Trace


pretty_tree :: Show e => Tree e -> String
pretty_tree tree =
    let to_str tree indent = case tree of
            Leaf -> indent ++ "|- "
            Node {elm=elm, children=children} ->
                indent ++ "|- " ++ (show elm) ++ "\n" ++ (intercalate "\n" (map (\c -> to_str c (indent ++ "|  ")) children))
    in
    to_str tree ""


pretty_time_tree :: Show e => Int -> TimeTree e -> String
pretty_time_tree time tree =
    let to_str tree time indent = case tree of
            TimeLeaf -> indent ++ "|- (" ++ (show time) ++ ")"
            TimeNode {t_elm=elm, t_fields=children} ->
                indent ++ "|- " ++ (show elm) ++ " (" ++ (show time) ++ ") \n" ++ (intercalate "\n" (map (\(t, c) -> to_str c t (indent ++ "|  ")) children))
    in
    to_str tree time ""
