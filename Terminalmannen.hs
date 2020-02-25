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
    , g,g,"Red",g,g,g,g
    , g,g,g,g,g,g,g
    , g,g,g,g,"Red",g,g
    , g,g,"Red","Red","Red","Red",g]
changeColor :: String -> Int -> [String] -> [String]
changeColor color x (k:cs) | x > 0 =  [k] ++ changeColor color (x-1) cs
                | otherwise = (color:cs)



--main = do
    --skriv hej skriv en int
    --hämta int
    --kalla på dropmannen med röd och int
    --skriv ut planen
    --kolla vinst röd
       -- om vinst, skriv ut skit och avbryt

    --blå:
    --skriv hej skriv blå int
    -- hämta int
    --kalla drop men int och blå
    --skriv ut planen
    --kolla vinst blå
         -- om vinst, skriv ut skit och avbryt
    --main



traverseList :: Int -> [String] -> String
traverseList x (c:cs) | x == length (c:cs) = c
                      | x <= 0 = c
                      | otherwise = traverseList (x-1) cs


dropmannen :: String -> Int ->  Int -> [String] -> [String]
dropmannen color dim x c | dim*7 == 42 = c
                         | (traverseList (((6-dim)*7)-(7-x))) c /= color = changeColor color (((6-dim)*7)-(7-x)) c
                         | otherwise = dropmannen  color (dim+1) x c 


checkWinColumn :: String -> Int -> [String] -> Bool
checkWinColumn color index c | (traverseList (index + 7) c == color && traverseList (index + 14) c == color && traverseList (index + 21) c == color) = True
                             | otherwise = False


--checkwinColumn2 är den rätta versionen. Inte helt testat klart än.
checkWinColumn2 :: String -> Int -> Int -> Int -> [String] -> Bool
checkWinColumn2 color row index tracker c | tracker == 3 = True
                              | index  == 41 = False
                              | index > 34 = checkWinColumn2 color (row + 1) (row + 1) 0 c
                              | traverseList index c == color = checkWinColumn2 color row (index + 7) (tracker + 1) c
                              | otherwise = checkWinColumn2 color row (index + 7) tracker c

--vill jättegärna kunna hålla koll på koordinaten där markören hamnar 


checkWinRow :: String -> Int -> Int -> [String] -> Bool
checkWinRow color index tracker c | tracker == 3 = True
                                 -- | index `mod` 7 == 6 = False -- ska egentligen göra : checkWinRow color index + 1 tracker=0 c
                                  | index == length c + 1 = False -- vet faktiskt inte
                                  | index `mod` 7 == 6 =  checkWinRow color (index + 1) 0 c
                                  | traverseList (index) c == color && traverseList (index + 1) c == color = (checkWinRow color (index + 1) (tracker + 1) c) 
                                  | otherwise = checkWinRow color (index +1) 0 c

--VAD FAN ÄR DE HÄR hela jävLA WINCHECK kan vara EN JÄvla FUNKTION dkljhfgjkldfgkjhdsfg
checkDiagonalRight :: String -> Int -> Int -> [String] -> Bool -- kolla nere same shit
checkDiagonalRight color index tracker c | tracker == 4 = True 
                                         | index `mod` 7 == 6 || index > 41 = False
                                         | traverseList (index) c == color = checkDiagonalRight color (index - 6) (tracker +1) c
                                         | otherwise = checkDiagonalRight color (index - 6) 0 c

checkDiagonalLeft :: String -> Int -> Int -> [String] -> Bool
checkDiagonalLeft color index tracker c | tracker == 4 = True -- vinst
                                        | index `mod` 7 == 0 || index < 0 = False -- sluta rekursera om vi kommit till column 1  eller om index < 0
                                        | traverseList (index) c == color = checkDiagonalLeft color (index - 8) (tracker +1) c   -- kolla nästa med +1 tracker
                                        | otherwise = checkDiagonalLeft color (index - 8) 0 c -- inte rätt färg, kolla nästa med nollställd tracker

-- testar alla hårdkodade positioner. Diagonal kan bara ske på vissa rader
checkDiagonalMannen :: String -> [String] -> Bool
checkDiagonalMannen color c | checkDiagonalRight color 21 0 c ||
 checkDiagonalRight color 28 0 c || 
 checkDiagonalRight color 35 0 c || 
 checkDiagonalRight color 36 0 c || 
 checkDiagonalRight color 37 0 c || 
 checkDiagonalRight color 38 0 c  = True
                             | checkDiagonalLeft color 27 0 c || checkDiagonalLeft color 34 0 c || checkDiagonalLeft color 41 0 c || checkDiagonalLeft color 40 0 c ||
 checkDiagonalLeft color 39 0 c ||
 checkDiagonalLeft color 38 0 c = True
                             | otherwise = False

                             