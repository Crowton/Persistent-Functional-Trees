module Binary_Tree_temporal where

import DataRecords


contains :: Ord e => e -> Tree e -> Bool
contains _ Leaf = False
contains e (Node {elm=elm, children=[left, right]}) =
    if e == elm
        then True
        else if e < elm
                then contains e left
                else contains e right


insert :: Ord e => e -> Tree e -> Tree e
insert e Leaf = Node {elm=e, children=[Leaf, Leaf]}
insert e Node {elm=elm, children=[left, right]} =
    if e == elm
        then Node {elm=elm, children=[left, right]}
        else if e < elm
                then Node {elm=elm, children=[insert e left, right]}
                else Node {elm=elm, children=[left, insert e right]}


extract_max :: Ord e => Tree e -> (Tree e, Maybe e)
extract_max Leaf = (Leaf, Nothing)
extract_max Node {elm=elm, children=[left, Leaf]} = (left, Just elm)
extract_max Node {elm=elm, children=[left, right]} =
    let (right', max) = extract_max right in
    (Node {elm=elm, children=[left, right']}, max)

delete :: Ord e => e -> Tree e -> Tree e
delete _ Leaf = Leaf
delete e Node {elm=elm, children=[left, right]} =
    if e == elm
        then let (left', max) = extract_max left in
             case max of
                 Nothing -> right
                 Just elm' -> Node {elm=elm', children=[left', right]}
        else if e < elm
                then Node {elm=elm, children=[delete e left, right]}
                else Node {elm=elm, children=[left, delete e right]}