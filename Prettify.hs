module Prettify where

import Data.String
import Data.List

import DataRecords



pretty_tree :: Show e => Tree e -> String
pretty_tree tree =
    let to_str tree indent = case tree of
            Leaf -> indent ++ "|- "
            Node {elm=elm, children=children} ->
                indent ++ "|- " ++ (show elm) ++ "\n" ++ (intercalate "\n" (map (\c -> to_str c (indent ++ "|  ")) children))
    in
    to_str tree ""
