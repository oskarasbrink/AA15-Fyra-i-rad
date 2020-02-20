--main :: IO ()
asd = let
    r = "Red"
    b = "Blue"
    c = makegray "grey" in
        dropmannen r 0 1 (dropmannen r 0 1 (dropmannen r 0 1 (dropmannen r 0 1 (dropmannen r 0 1 (dropmannen r 0 1 (dropmannen r 0 1 c))))))
--g = "Gray"



makegray :: String -> [String]
makegray g = [g,g,g,g,g,g,g
    , g,g,g,g,g,g,g
    , g,g,g,g,g,g,g
    , g,g,g,g,g,g,g
    , g,g,g,g,g,g,ge
    , g,g,g,g,g,g,g]
changeColor :: String -> Int -> [String] -> [String]
changeColor color x (k:cs) | x > 0 =  [k] ++ changeColor color (x-1) cs
                | otherwise = (color:cs)



traverseList :: Int -> [String] -> String
traverseList x (c:cs) | x == 0 = c
                      | otherwise = traverseList (x-1) cs


dropmannen :: String -> Int ->  Int -> [String] -> [String]
dropmannen color dim x c | dim*7 == 42 = c
                         | (traverseList (((6-dim)*7)-(7-x))) c == "grey" = changeColor color (((6-dim)*7)-(7-x)) c
                         | otherwise = dropmannen  color (dim+1) x c 