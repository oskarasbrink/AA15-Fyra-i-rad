--main :: IO ()
asd = let
    r = "Red"
    b = "Blue"
    c = makegray "grey" in
        dropmannen r 0 1 (dropmannen r 0 1 (dropmannen r 0 1 (dropmannen r 0 3 (dropmannen r 0 4 (dropmannen r 0 5 (dropmannen r 0 6 c))))))

asd2 = let
    r = "Red"
    b = "Blue"
    c = makegray "grey" in
        dropmannen r 0 1 (dropmannen r 0 6 (dropmannen r 0 5 (dropmannen r 0 4 (dropmannen r 0 3 (dropmannen r 0 2 (dropmannen r 0 1 c))))))
--g = "Gray"



makegray :: String -> [String]
makegray g = [g,g,g,g,g,g,g
    , g,g,g,g,g,g,g
    , g,g,g,g,g,g,g
    , g,g,g,g,g,g,g
    , g,g,g,g,g,g,g
    , g,g,g,g,g,g,g]
changeColor :: String -> Int -> [String] -> [String]
changeColor color x (k:cs) | x > 0 =  [k] ++ changeColor color (x-1) cs
                | otherwise = (color:cs)



traverseList :: Int -> [String] -> String
traverseList x (c:cs) | x == length (c:cs) = c
                      | x <= 0 = c
                      | otherwise = traverseList (x-1) cs


dropmannen :: String -> Int ->  Int -> [String] -> [String]
dropmannen color dim x c | dim*7 == 42 = c
                         | (traverseList (((6-dim)*7)-(7-x))) c == "grey" = changeColor color (((6-dim)*7)-(7-x)) c
                         | otherwise = dropmannen  color (dim+1) x c 


checkWinColumn :: String -> Int -> [String] -> Bool
checkWinColumn color index c | (traverseList (index + 7) c == color && traverseList (index + 14) c == color && traverseList (index + 21) c == color) = True
                             | otherwise = False


--vill jättegärna kunna hålla koll på koordinaten där markören hamnar 
checkWinRow :: String -> Int -> Int -> [String] -> Bool
checkWinRow color index tracker c | tracker == 4 = True
                                  | index `mod` 7 == 6 = False
                                  | index == length c + 1= False
                                  | traverseList (index) c == color && traverseList (index + 1) c == color = (checkWinRow color (index + 1) (tracker + 1) c) 
                                  | otherwise = checkWinRow color (index +1) 0 c