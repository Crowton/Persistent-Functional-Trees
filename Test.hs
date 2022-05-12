import Data.List

split :: [a] -> ([a], [a])
split myList = splitAt ((length myList) `div` 2) myList
main = do
        print $ split [1, 2, 3, 5, 6]
        print $ split [1, 2, 3, 4, 5, 6]
