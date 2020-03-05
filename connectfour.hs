
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss
import Test.HUnit
import Debug.Trace
type GameState = [Color]
type Arrow = (Int,(Float,Float))
type Player = Int
type World = (Score,((Player,Arrow),GameState))
type Score = (Int,Int)

main =
   play windowDisplay black 5 ((0,0),(((1,(0,((-300),320)))),(generateBoard'))) drawingFunc inputHandler' (const id)



{- inputHandler' event world
   handles the user inputs and depending on key pressed changes the gamestate accordingly, ie moves arrow one step left or right, places marker or resets the board
   RETURNS: New World depending on key pressed
  -}
inputHandler' :: Event -> World -> World
inputHandler' (EventKey (SpecialKey KeyEnter) Down _ _) (score,((_,(index,t)),gs)) = (score,((1,(index,t)),generateBoard'))
inputHandler' (EventKey (SpecialKey KeySpace) Down _ _) (score,((x,(index,t)),(p:xs))) | x == 2 = (score,((x,(index,t)),(p:xs)))
                                                                           | checkWin (colorFunction  x) (dropFunction (colorFunction x) 0 index (p:xs)) = ((increaseScore (colorFunction x) score),((2,(index,t)),(dropFunction (colorFunction x) 0 index (p:xs))))
                                                                           | index < 7 && traverseList index (p:xs) /= (greyN 0.5) = (score,((x,(index,t)),(p:xs)))
                                                                           | otherwise = (score,(((-1)*x,(index,t)),(dropFunction (colorFunction x) 0 index (p:xs)))) 
inputHandler' (EventKey (SpecialKey KeyRight) Down _ _) (score,((x,t1),gs)) = (score,((x,(plusArrowIndex t1)),gs))
inputHandler' (EventKey (SpecialKey KeyLeft) Down _ _) (score,((x,t1),gs)) = (score,((x,(minusArrowIndex t1)),gs))
inputHandler' _ (score,((x,(index,t)),gs)) = (score,((x,(index,t)),gs))

 
--increases score on scoreboard depending on winning color.
increaseScore:: Color -> Score -> Score
increaseScore color (x,y) |  color == red = ((x+1),y)
                          |  color == blue = (x,(y+1))
   

{- plusArrowIndex Arrow
   Moves the small circle (our "arrow") one step to the right with each KeyRight pressed, stops at index 6
   RETURNS: Arrow with updated coordinates
   PRE: 0 <= index <= 6
   EXAMPLES: plusArrowIndex (1,((-300),320)) == (2,((-150),320))
             plusArrowIndex (6,((600),320)) == (6,((600),320))
  -}

plusArrowIndex :: Arrow -> Arrow
plusArrowIndex (index,(x,y)) | index == 6 = (6,(x,y))
                        | otherwise = ((index + 1),((x + 150 ),y))

{- minusArrowIndex Arrow
   Moves the small circle (our arrow) one step to the left with each KeyLeft pressed. stops at index 0.
   RETURNS: Arrow with updated coordinates
   PRE: 0 <= index <= 6
   EXAMPLES: minusArrowIndex (2,((-150),320)) == (1,((-300),320))
             minusArrowIndex (0,((-300),320)) == (0,((-300),320)) 
  -}

minusArrowIndex :: Arrow -> Arrow
minusArrowIndex (index,(x,y)) | index == 0 = (0,(x,y))
                        | otherwise = ((index - 1),((x - 150) ,y))

--returns color depending on value of x
colorFunction :: Int -> Color 
colorFunction x | x == 2 = green
                | x == 1 = red
                | otherwise  = blue


{- changeColor color index gameState
   changes color of an item at certain index x into Color color
   RETURNS: GameState with updated color at index x
   PRE: 0 <= x < length gameState
   EXAMPLES: changeColor red 0 [blue,blue] == [red,blue]
-}

--  VARIANT: length cs
changeColor :: Color -> Int -> GameState -> GameState
changeColor color x (c:cs) | x > 0 = [c] ++ changeColor color (x-1) cs
                           | otherwise = (color:cs)   

--generates empty board           
generateBoard' :: GameState
generateBoard' = [(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5)]


{- traverseList x gameState
   Fetches the color of an item on a certain index in a GameState
   RETURNS: color of item on index x in GameState
   PRE: 0 <= x < length GameState
   EXAMPLES: traverseList 1 (changeColor red 1 generateBoard') == red
             traverseList 1 generateBoard' == greyN 0.5
  -}

-- VARIANT: length cs
traverseList :: Int -> GameState -> Color
traverseList x (c:cs) | x == length (c:cs) = c
                      | cs == [] = c
                      | x <= 0 = c
                      | otherwise = traverseList (x-1) cs

{- dropFunction color dim x gs
   Drops the marker on the top most available slot in the column with index x
   RETURNS: An updated GameState
   PRE: dim = 0 
        length gs == 42
        x <= 6

   EXAMPLES: traverseList 33 (dropFunction red 0 5(dropFunction blue 0 5 (generateBoard'))) == red
  -}

--VARIANT: length gs
dropFunction :: Color -> Int -> Int -> GameState -> GameState
dropFunction color dim x gs | dim * 7 == 42 =  gs
                             | (traverseList (((6-dim)*7)-(7-x)) gs) == greyN 0.5 = changeColor color (((6-dim)*7)-(7-x)) gs
                             | otherwise = dropFunction  color (dim+1) x gs



{- checkWinColumn color row index tracker gs
   Tracks how many circles of the same color that are in a row vertically
   RETURNS: True if tracker = 3
   PRE: 0 <= index <= 41
        row = 0
        tracker = 0
        length gs = 42
   EXAMPLES: checkWinColumn red 0 0 0 (dropFunction red 0 5(dropFunction red 0 5(dropFunction red 0 5(dropFunction red 0 5 (generateBoard'))))) == True
  -}

--VARIANT: length gs
checkWinColumn :: Color -> Int -> Int -> Int -> GameState -> Bool
checkWinColumn color row index tracker gs | tracker == 3 = True
                                              | index == 41 = False
                                              | index > 34 = checkWinColumn color (row +1) (row + 1) 0 gs 
                                              | traverseList index gs == color && traverseList (index + 7) gs == color = checkWinColumn color row (index + 7) (tracker +1) gs
                                              | otherwise = checkWinColumn color row (index + 7) 0 gs


{- checkWinRow color index tracker gs
   Checks for 4 in a row on a horizontal level and returns True if 4 elements of the same color are next to each other
   RETURNS: True if tracker = 3
   PRE: index = 0
        tracker = 0
        length gs = 42
   EXAMPLES: checkWinRow red 0 0 (dropFunction red 0 0(dropFunction red 0 1(dropFunction red 0 2(dropFunction red 0 3(dropFunction red 0 4(generateBoard')))))) == True
             checkWinRow red 0 0 (dropFunction red 0 0(dropFunction red 0 1(dropFunction blue 0 2(dropFunction red 0 3(dropFunction red 0 4(dropFunction red 0 5 (generateBoard'))))))) == False
  -}

--VARIANT: VARIANT: length gs
checkWinRow :: Color -> Int -> Int -> GameState -> Bool
checkWinRow color index tracker gs | tracker == 3 = True
                                      | index == length gs + 1 = False
                                      | index `mod` 7 == 6 = checkWinRow color (index + 1) 0 gs
                                      | traverseList (index) gs == color && traverseList (index +1 ) gs == color = checkWinRow color (index + 1) (tracker +1) gs
                                      | otherwise = checkWinRow color (index + 1) 0 gs

{- checkDiagonalRight color index tracker gs
   Tracks if there are 4 circles of the same color diagonally upwards from left to right
   RETURNS: True if tracker = 3
   PRE: index < length gs
        tracker = 0
        length gs = 42
   EXAMPLES: formatting made for easier reading, need correct indentation to work in terminal. see testcases 15 & 16 for correct formatting.
             checkDiagonalRight red 36 0 (dropFunction red 0 4(dropFunction blue 0 3(dropFunction red 0 4(dropFunction blue 0 4(dropFunction red 0 3(
                                dropFunction blue 0 4(dropFunction red 0 3(dropFunction blue 0 4(dropFunction red 0 3(dropFunction blue 0 3
                                (dropFunction red 0 2(dropFunction blue 0 2(dropFunction red 0 1(generateBoard')))))))))))))) == True
             checkDiagonalRight red 36 0 (dropFunction red 0 4(dropFunction blue 0 5(dropFunction red 0 4(dropFunction blue 0 4(dropFunction red 0 4
                                (dropFunction blue 0 3(dropFunction red 0 3(dropFunction blue 0 3(dropFunction red 0 2(dropFunction blue 0 2(dropFunction red 0 1(generateBoard')))))))))))) == False

  -}

--VARIANT: length gs
checkDiagonalRight :: Color -> Int -> Int -> GameState -> Bool
checkDiagonalRight color index tracker gs | tracker == 3 = True
                                             | index `mod`7 == 6 || index < 0 = False
                                             | traverseList index gs == color && traverseList (index - 6) gs == color  = checkDiagonalRight color (index -6) (tracker + 1) gs
                                             | otherwise = checkDiagonalRight color (index - 6) 0 gs

{- checkDiagonalLeft color index tracker gs
   Tracks if there are 4 circles of the same color diagonally upwards from right to left
   RETURNS: True if tracker = 3
   EXAMPLES: formatting made for easier reading, need correct indentation to work in terminal. see testcases 17 & 18 for correct formatting
             checkDiagonalLeft red 38 0 (dropFunction red 0 0(dropFunction blue 0 1(dropFunction red 0 0(dropFunction blue 0 0
                              (dropFunction red 0 1(dropFunction blue 0 0(dropFunction red 0 1(dropFunction blue 0 1(dropFunction red 0 2
                              (dropFunction blue 0 2(dropFunction red 0 3(generateBoard'))))))))))))) == True
             checkDiagonalLeft blue 38 0 (dropFunction red 0 4(dropFunction blue 0 3(dropFunction red 0 4(dropFunction blue 0 4(dropFunction red 0 3(
                              dropFunction blue 0 4(dropFunction red 0 3(dropFunction blue 0 4(dropFunction red 0 3(dropFunction blue 0 3(dropFunction red 0 2(
                              dropFunction blue 0 2(dropFunction red 0 1(generateBoard')))))))))))))) == False
             
  -}
--VARIANT: length gs
checkDiagonalLeft :: Color -> Int -> Int -> GameState -> Bool
checkDiagonalLeft color index tracker gs | tracker == 3 = True
                                            | index `mod` 7 == 0 || index < 0 = False
                                            | traverseList index gs == color && traverseList (index - 8) gs == color = checkDiagonalLeft color (index - 8) (tracker +1 ) gs
                                            | otherwise = checkDiagonalLeft color (index - 8) 0 gs

{- checkDiagonal color gs
   Checks if diagonal win condition has been fulfilled after every dropped circle. Checks every possible diagonal win with help of hardcoded checkDiagonalLeft and checkDiagonalRight indexes.
   PRE: length gs = 42
   RETURNS: True if four diagonal elements are of the same color
   EXAMPLES: see checkDiagonalLeft and checkDiagonalRight  
  -}
  
checkDiagonal :: Color -> GameState -> Bool
checkDiagonal color gs | checkDiagonalRight color 21 0 gs ||
 checkDiagonalRight color 28 0 gs || 
 checkDiagonalRight color 35 0 gs || 
 checkDiagonalRight color 36 0 gs || 
 checkDiagonalRight color 37 0 gs || 
 checkDiagonalRight color 38 0 gs  = True
                             | checkDiagonalLeft color 27 0 gs || checkDiagonalLeft color 34 0 gs || checkDiagonalLeft color 41 0 gs || checkDiagonalLeft color 40 0 gs ||
 checkDiagonalLeft color 39 0 gs ||
 checkDiagonalLeft color 38 0 gs = True
                             | otherwise = False

{- checkWin color gs
   Goes through every check-function after each use of dropFunction and if one of them returns True, then checkWin returns True 
   RETURNS: True if any of checkWin-functions returns true.
   PRE: length gs = 42
   EXAMPLES: checkWin red (generateBoard') == False
             checkWin red (dropFunction red 0 0(dropFunction red 0 1(dropFunction red 0 2(dropFunction red 0 3(dropFunction red 0 4(generateBoard')))))) == True
             
  -}
checkWin :: Color -> GameState -> Bool
checkWin color gs | checkDiagonal color gs || checkWinRow color 0 0 gs  || checkWinColumn color 0 0 0 gs = True
               | otherwise = False 


windowDisplay :: Display
windowDisplay = InWindow "Window" (1440, 900) (0,0)

{- mkSmallCircle col x y
   Creates a small coloured circle at a pre-determined position 
   RETURNS: Picture at coordinates (x,y)
  -}

mkSmallCircle :: Color -> Float -> Float -> Picture
mkSmallCircle col x y = pictures [translate x y $ color col $ circleSolid 13]

{- mkCircle col x y
   Creates a coloured circle at a pre-determined position 
   RETURNS: Picture at coordinates (x,y)
  -}

mkCircle :: Color -> Float -> Float -> Picture
mkCircle col x y = pictures [translate x y $ color col $ circleSolid 26]

{- mkScore x y
   Creates Picture of two integers
   RETURNS: Picture of integers x and y
  -}

mkScore :: Score -> Float -> Float -> Picture
mkScore (x,y) z q = pictures [translate z q $ color red $ text (show x) ,translate (z-100) (q-100) $ color blue $ text (show y)]


{- getcolor index gs
   fetches the color of an item at a certain index in a GameState 
   RETURNS: color of item at specified index of GameState
   EXAMPLES: getcolor 2 (changeColor red 2 (generateBoard')) == red
             getcolor 0 generateBoard' == greyN 0.5
  -}

--VARIANT: length gs
getcolor :: Int -> GameState -> Color
getcolor  index (x:xs) | index == 0 = x                  
                       | otherwise = (getcolor (index -1) xs)


{- drawingFunc ((x,(index, (t1,t2))), gs)
   Creates a graphic visualization of a "4 in a row" game with circles with colors varying with the gamestate
   PRE: length gs = 42
   RETURNS: Graphic represantiotion och a connect-four-board
  -}
drawingFunc :: World -> Picture
drawingFunc (score,((x,(index,(t1,t2))),gs)) = pictures [rectangleSolid 1400 900, mkCircle (getcolor 16 gs ) 0 40, mkCircle (getcolor 10 gs) 150 140, mkCircle (getcolor 4 gs)  300 240, mkCircle (getcolor 5 gs ) 450 240, mkCircle (getcolor 11 gs ) 300 140, 
                                                           mkCircle (getcolor 9 gs ) 0 140, mkCircle (getcolor 2 gs ) 0 240, mkCircle (getcolor 23 gs ) 0 (-60), mkCircle (getcolor 3 gs ) 150 240, mkCircle (getcolor 12 gs ) 450 140,
                                                           mkCircle (getcolor 30 gs ) 0 (-160), mkCircle (getcolor 37 gs ) 0 (-260), mkCircle (getcolor 17 gs ) 150 40, mkCircle (getcolor 24 gs ) 150 (-60), mkCircle (getcolor 31 gs) 150 (-160), mkCircle (getcolor 38 gs ) 150 (-260),
                                                           mkCircle (getcolor 18 gs ) 300 40, mkCircle (getcolor 25 gs ) 300 (-60), mkCircle (getcolor 32 gs ) 300 (-160), mkCircle (getcolor 39 gs ) 300 (-260),
                                                           mkCircle (getcolor 19 gs ) 450 40, mkCircle (getcolor 26 gs ) 450 (-60), mkCircle (getcolor 33 gs ) 450 (-160), mkCircle (getcolor 40 gs ) 450 (-260),
                                                           mkCircle (getcolor 6 gs ) 600 240, mkCircle (getcolor 13 gs ) 600 140, mkCircle (getcolor 20 gs ) 600 40, mkCircle (getcolor 27 gs ) 600 (-60), mkCircle (getcolor 34 gs ) 600 (-160), mkCircle (getcolor 41 gs ) 600 (-260),
                                                           mkCircle (getcolor 1 gs ) (-150) 240, mkCircle (getcolor 8 gs ) (-150) 140, mkCircle (getcolor 15 gs ) (-150) 40, mkCircle (getcolor 22 gs )  (-150) (-60), mkCircle (getcolor 29 gs ) (-150) (-160), mkCircle (getcolor 36 gs ) (-150) (-260),
                                                           mkCircle (getcolor 0 gs ) (-300) 240, mkCircle (getcolor 7 gs ) (-300) 140, mkCircle (getcolor 14 gs ) (-300) 40, mkCircle (getcolor 21 gs ) (-300) (-60), mkCircle (getcolor 28 gs ) (-300) (-160), mkCircle (getcolor 35 gs ) (-300) (-260), mkSmallCircle (colorFunction x) t1 t2,mkScore score (-500) 240]--,mkSmallCircle green (-500) 240]




test1 = TestCase $ assertEqual "grey square "
            (greyN 0.5) (getcolor 0 generateBoard')

test28 = TestCase $ assertEqual "color red square index 2s"
            (red) (getcolor 2 (changeColor red 2 (generateBoard')))



test2 = TestCase $ assertEqual "plusArrowIndex"
            ((5,(150,0))) (plusArrowIndex (4,(0,0)))

test3 = TestCase $ assertEqual "ArrowIndex edge"
            ((6,(0,0))) (plusArrowIndex (6,(0,0)))

test4 = TestCase $ assertEqual "minusArrowIndex"
            ((0,(150,0))) (minusArrowIndex (0,(150,0)))

test5 = TestCase $ assertEqual "minusArrowIndex edge"
            ((0,(150,0))) (minusArrowIndex (0,(150,0)))

--traverselist kommer ge color om den får negativt index
test6 = TestCase $ assertEqual "traverseList"
            (greyN 0.5) (traverseList 1 generateBoard')

test7 = TestCase $ assertEqual "traverseList"
            (red) (traverseList 1 (changeColor red 1 generateBoard'))

test8 = TestCase $ assertEqual "dropFunction test 1"
            (blue) (traverseList 40 (dropFunction blue 0 5 (generateBoard')))

test9 = TestCase $ assertEqual "dropFunction test 2"
            (red) (traverseList 33 (dropFunction red 0 5(dropFunction blue 0 5 (generateBoard'))))

--detta ska vara out of bounds test
test10 = TestCase $ assertEqual "dropFunction test 2"
            (red) (traverseList 33 (dropFunction red 0 5(dropFunction blue 0 5 (generateBoard'))))

test11 = TestCase $ assertEqual "checkWinColumn 1"
            (True) (checkWinColumn red 0 0 0 (dropFunction red 0 5(dropFunction red 0 5(dropFunction red 0 5(dropFunction red 0 5 (generateBoard'))))))

test12 = TestCase $ assertEqual "checkWinColumn 2. Red-Red-Blue-Red-Red in a column"
            (False) (checkWinColumn red 0 0 0 (dropFunction red 0 5(dropFunction red 0 5(dropFunction blue 0 5(dropFunction red 0 5(dropFunction red 0 5(dropFunction red 0 5 (generateBoard'))))))))

test13 = TestCase $ assertEqual "checkWinRow red"
            (True) (checkWinRow red 0 0 (dropFunction red 0 0(dropFunction red 0 1(dropFunction red 0 2(dropFunction red 0 3(dropFunction red 0 4(generateBoard')))))))


test14 = TestCase $ assertEqual "checkWinRow red"
           (False) (checkWinRow red 0 0 (dropFunction red 0 0(dropFunction red 0 1(dropFunction blue 0 2(dropFunction red 0 3(dropFunction red 0 4(dropFunction red 0 5 (generateBoard'))))))))

test15 = TestCase $ assertEqual "checkDiagonal right red from index 36"
           (True) (checkDiagonalRight red 36 0 (dropFunction red 0 4(dropFunction blue 0 3(dropFunction red 0 4(dropFunction blue 0 4(dropFunction red 0 3(dropFunction blue 0 4(dropFunction red 0 3(dropFunction blue 0 4(dropFunction red 0 3(dropFunction blue 0 3(dropFunction red 0 2(dropFunction blue 0 2(dropFunction red 0 1(generateBoard')))))))))))))))

test16 = TestCase $ assertEqual "checkDiagonal right red-red-blue-red from index 36"
           (False) (checkDiagonalRight red 36 0 (dropFunction red 0 4(dropFunction blue 0 5(dropFunction red 0 4(dropFunction blue 0 4(dropFunction red 0 4(dropFunction blue 0 3(dropFunction red 0 3(dropFunction blue 0 3(dropFunction red 0 2(dropFunction blue 0 2(dropFunction red 0 1(generateBoard')))))))))))))


test17 = TestCase $ assertEqual "checkDiagonal left red from index 38"
           (True) (checkDiagonalLeft red 38 0 (dropFunction red 0 0(dropFunction blue 0 1(dropFunction red 0 0(dropFunction blue 0 0(dropFunction red 0 1(dropFunction blue 0 0(dropFunction red 0 1(dropFunction blue 0 1(dropFunction red 0 2(dropFunction blue 0 2(dropFunction red 0 3(generateBoard')))))))))))))

test18 = TestCase $ assertEqual "checkDiagonal left red-red-red-blue-red from index 38"
           (False) (checkDiagonalLeft red 38 0 (dropFunction red 0 0(dropFunction blue 0 1(dropFunction red 0 0(dropFunction blue 0 0(dropFunction blue 0 1(dropFunction blue 0 0(dropFunction red 0 1(dropFunction blue 0 1(dropFunction red 0 2(dropFunction blue 0 2(dropFunction red 0 3(generateBoard')))))))))))))

test19 = TestCase $ assertEqual "check blue when four in a row diagonal right red"
           (False) (checkDiagonal blue (dropFunction red 0 4(dropFunction blue 0 3(dropFunction red 0 4(dropFunction blue 0 4(dropFunction red 0 3(dropFunction blue 0 4(dropFunction red 0 3(dropFunction blue 0 4(dropFunction red 0 3(dropFunction blue 0 3(dropFunction red 0 2(dropFunction blue 0 2(dropFunction red 0 1(generateBoard')))))))))))))))

test20 = TestCase $ assertEqual "check blue when four in a row diagonal left red"
           (False) (checkDiagonal blue (dropFunction red 0 0(dropFunction blue 0 1(dropFunction red 0 0(dropFunction blue 0 0(dropFunction red 0 1(dropFunction blue 0 0(dropFunction red 0 1(dropFunction blue 0 1(dropFunction red 0 2(dropFunction blue 0 2(dropFunction red 0 3(generateBoard')))))))))))))

test21 = TestCase $ assertEqual "checkWin red from index 38"
           (True) (checkWin red (dropFunction red 0 0(dropFunction blue 0 1(dropFunction red 0 0(dropFunction blue 0 0(dropFunction red 0 1(dropFunction blue 0 0(dropFunction red 0 1(dropFunction blue 0 1(dropFunction red 0 2(dropFunction blue 0 2(dropFunction red 0 3(generateBoard')))))))))))))

test22 = TestCase $ assertEqual "checkWin red row"
            (True) (checkWin red (dropFunction red 0 0(dropFunction red 0 1(dropFunction red 0 2(dropFunction red 0 3(dropFunction red 0 4(generateBoard')))))))

test23 = TestCase $ assertEqual "checkWin empty board"
            (False) (checkWin red (generateBoard'))


test24 = TestCase $ assertEqual "ColorFunction"
            (green) (colorFunction 2)
test25 = TestCase $ assertEqual "ColorFunction"
            (red) (colorFunction 1)
test26 = TestCase $ assertEqual "ColorFunction"
            (blue) (colorFunction (-1))
test27 = TestCase $ assertEqual "ColorFunction"
            (blue) (colorFunction (-5))

--lägg till att om dropFunction är negativ eller ur index att den ska skicka tillbaks
--ursprungliga gamestate

--test12 = TestCase $ assertEqual "traverseList"
--            (red) (traverseList 33 (dropFunction red 0 5(dropFunction blue 0 5 (generateBoard'))))


--test6 = TestCase $ assertEqual "basic changecolor"
--            ()) (minusArrowIndex (0,(150,0)))

--test2 = TestCase $ assertEqual "codeTable"
--            3 (maybe (-1) length (Table.lookup (codeTable (huffmanTree (characterCounts "this is an example of a huffman tree"))) ' '))

--test3 = TestCase $ assertEqual "compress"
--            135 (length (snd (compress "this is an example of a huffman tree")))

{-
test4 =
    let s = "this is an example of a huffman tree"
    in
      TestCase $ assertEqual ("decompress \"" ++ s ++ "\"")
        s (let (h, bits) = compress s in decompress h bits)

test5 =
    let s = "xxx"
    in
      TestCase $ assertEqual ("decompress \"" ++ s ++ "\"")
        s (let (h, bits) = compress s in decompress h bits)

test6 =
    let s = ""
    in
      TestCase $ assertEqual ("decompress \"" ++ s ++ "\"")
        s (let (h, bits) = compress s in decompress h bits)

-}

runtests = runTestTT $ TestList [
   test1, test2, test3, test4,test5,test6,test7,test8,
   test9,test10,test11,test12,test13,test14,test15,test16,test17,test18,test19,test20,test21,test22,test23,test24,test25,test26,test27,test28
   ]



--ågge testcases
--ågge fixa local functions
--olika filer imorgon
--jeppe rapport



-- diagonaltestcases
-- fixa och testa out of bounds-värden
-- ska vi ha olika filer?
-- rapport
-- scorescreen? är nog inte så svårt
