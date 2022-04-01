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


print_TimeTree :: Show e => TimeTree e -> Bool
print_TimeTree TimeLeaf = True
print_TimeTree TimeNode {t_elm=t_elm, t_fields=[(left_time, left_tree), (right_time, right_tree)]} =
    trace ("Visiting node " ++ (show t_elm)) $!
    True
