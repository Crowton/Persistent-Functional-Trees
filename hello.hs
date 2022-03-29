fact :: Int -> Int
fact 0 = 1
fact n = n * (fact (n - 1))

charToInt :: Char -> Int
charToInt '0' = 0
charToInt '1' = 1
charToInt '2' = 2
charToInt '3' = 3
charToInt '4' = 4
charToInt '5' = 5
charToInt '6' = 6
charToInt '7' = 7
charToInt '8' = 8
charToInt '9' = 9

toIntAux :: [Char] -> Int
toIntAux [] = 0
toIntAux (c : cs) = (charToInt c) + 10 * (toIntAux cs)

toInt :: [Char] -> Int
toInt cs = toIntAux (reverse cs)

main = do
    putStrLn "Hello World!"
    print (fact 5)
    print ((\x -> x + 1) 4)
    print (toInt "409473")
