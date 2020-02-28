--main :: IO ()

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss
--type GameState = [((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String)]
type GameState = [((Float,Float),Color)]
main :: IO ()
main = do
    speltillstond <- generateBoard 
    putStrLn "blue player"
    spela speltillstond
    
--display :: Display -> Color -> Picture -> IO
spela speltillstond = do
    display ( InWindow "Nice Window" (200, 200) (10, 10)) white (ohmygoddrawingFunc speltillstond)
    


{-
main :: IO ()
main = do 
  putStrLn "Welcome to Nim."
  gameState <- genGameState 
  play gameState
-}
{-
play gameState = do
  printGameState gameState
  newGameState <- playerMove gameState
  if victory newGameState then do
    putStrLn "Player won!"
    putStrLn ""
    main
   else do
    newNewGameState <- computerMove newGameState  
    if victory newNewGameState then do
      putStrLn "Computer won!"
      putStrLn ""
      main
     else
      play newNewGameState      



-}

asd = let
    r = "Red"
    b = "Blue"
    c = makegreyN "greyN" in
        dropmannen r 0 1 (dropmannen r 0 1 (dropmannen r 0 1 (dropmannen r 0 3 (dropmannen r 0 4 (dropmannen r 0 5 (dropmannen r 0 6 c))))))

asd2 = let
    r = "Red"
    b = "Blue"
    c = makegreyN "greyN" in
        dropmannen r 0 1 (dropmannen r 0 6 (dropmannen r 0 5 (dropmannen r 0 4 (dropmannen r 0 3 (dropmannen r 0 2 (dropmannen r 0 1 c))))))
--g = "greyN"



makegreyN :: String -> [String]
makegreyN g = [g,g,g,g,g,g,g
    , g,g,g,g,g,g,g
    , g,g,"Red",g,g,g,g
    , g,g,g,g,g,g,g
    , g,g,g,g,"Red",g,g
    , g,g,"Red","Red","Red","Red",g]
changeColor :: String -> Int -> [String] -> [String]
changeColor color x (k:cs) | x > 0 =  [k] ++ changeColor color (x-1) cs
                | otherwise = (color:cs)


generateBoard ::IO GameState
generateBoard = return [((0,0),greyN),((0,0),greyN),((0,0),greyN),((0,0),greyN),((0,0),greyN),((0,0),greyN),((0,0),greyN),((0,0),greyN),((0,0),greyN),((0,0),greyN),((0,0),greyN),((0,0),greyN),((0,0),greyN),((0,0),greyN),((0,0),greyN),((0,0),greyN),((0,0),greyN),((0,0),greyN),((0,0),greyN),((0,0),greyN),((0,0),greyN),((0,0),greyN),((0,0),greyN),((0,0),greyN),((0,0),greyN),((0,0),greyN),((0,0),greyN),((0,0),greyN),((0,0),greyN),((0,0),greyN),((0,0),greyN),((0,0),greyN),((0,0),greyN),((0,0),greyN),((0,0),greyN),((0,0),greyN),((0,0),greyN),((0,0),greyN),((0,0),greyN),((0,0),greyN),((0,0),greyN),((0,0),greyN)]


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


newTraverseList :: Int -> GameState -> Color
newTraverseList x (c:cs) | x == length (c:cs) = snd c
                         | x <= 0 = snd c
                         | otherwise = newTraverseList (x-1) cs


dropmannen :: String -> Int ->  Int -> [String] -> [String]
dropmannen color dim x c | dim*7 == 42 = c
                         | (traverseList (((6-dim)*7)-(7-x))) c /= color = changeColor color (((6-dim)*7)-(7-x)) c
                         | otherwise = dropmannen  color (dim+1) x c 

newDropMannen :: Color -> Int -> Int -> GameState -> GameState
newDropMannen color dim x gs | dim * 7 == 42 = gs
                          | (traverseList (((6-dim)*7)-(7-x))) gs /= color = changeColor color (((6-dim)*7)-(7-x)) gs


checkWinColumn :: String -> Int -> [String] -> Bool
checkWinColumn color index c | (traverseList (index + 7) c == color && traverseList (index + 14) c == color && traverseList (index + 21) c == color) = True
                             | otherwise = False

newCheckWinColumn2 :: Color -> Int -> Int -> Int -> GameState -> Bool
newCheckWinColumn2 color row index tracker gs | tracker == 3 = True
                                              | index == 41 = False
                                              | index > 34 = newCheckWinColumn2 color (row +1) (row + 1) 0
                                              | newTraverseList index gs == color = newCheckWinColumn2 color row (index + 7) (tracker +1) gs
                                              | otherwise = newCheckWinColumn2 color row (index + 7) tracker gs -- AKTA DIG



--checkwinColumn2 är den rätta versionen. Inte helt testat klart
checkWinColumn2 :: String -> Int -> Int -> Int -> [String] -> Bool
checkWinColumn2 color row index tracker c | tracker == 3 = True
                              | index  == 41 = False
                              | index > 34 = checkWinColumn2 color (row + 1) (row + 1) 0 c
                              | traverseList index c == color = checkWinColumn2 color row (index + 7) (tracker + 1) c
                              | otherwise = checkWinColumn2 color row (index + 7) tracker c



--vill jättegärna kunna hålla koll på koordinaten där markören hamnar 

newCheckWinRow :: Color -> Int -> Int -> GameState -> Bool
newCheckWinRow color index tracker gs | tracker == 3 = True
                                      | index == length gs + 1 = False
                                      | index `mod` 7 == 6 = newCheckWinRow color (index + 1) 0 gs
                                      | newTraverseList (index) gs == color && newTraverseList (index +1 ) gs == color = (newCheckWinRow color (index + 1) (tracker +1))
                                      | otherwise = newCheckWinRow color (index + 1) gs

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

newCheckDiagonalRight :: Color -> Int -> Int -> GameState -> Bool
newCheckDiagonalRight color index tracker gs | tracker == 4 = True
                                             | index `mod`7 == 6 || index > 41 = False
                                             | newTraverseList index gs == color = newCheckDiagonalRight color (index -6) (tracker + 1) gs
                                             | otherwise = newCheckDiagonalRight color (index - 6) 0 gs

checkDiagonalLeft :: String -> Int -> Int -> [String] -> Bool
checkDiagonalLeft color index tracker c | tracker == 4 = True -- vinst
                                        | index `mod` 7 == 0 || index < 0 = False -- sluta rekursera om vi kommit till column 1  eller om index < 0
                                        | traverseList (index) c == color = checkDiagonalLeft color (index - 8) (tracker +1) c   -- kolla nästa med +1 tracker
                                        | otherwise = checkDiagonalLeft color (index - 8) 0 c -- inte rätt färg, kolla nästa med nollställd tracker

newCheckDiagonalLeft :: Color -> Int -> Int -> GameState -> Bool
newCheckDiagonalLeft color index tracker gs | tracker == 4 = True
                                            | index `mod` 7 == 0 || index < 0 = False
                                            | newTraverseList index gs == color = newCheckDiagonalLeft color (index - 8) (tracker +1 ) gs
                                            | otherwise = newCheckDiagonalLeft color (index - 8) 0 gs

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

newCheckDiagonalMannen :: Color -> GameState -> Bool
newCheckDiagonalMannen color gs | newCheckDiagonalRight color 21 0 gs ||
 newCheckDiagonalRight color 28 0 gs || 
 newCheckDiagonalRight color 35 0 gs || 
 newCheckDiagonalRight color 36 0 gs || 
 newCheckDiagonalRight color 37 0 gs || 
 newCheckDiagonalRight color 38 0 gs  = True
                             | newCheckDiagonalLeft color 27 0 gs || newCheckDiagonalLeft color 34 0 gs || newCheckDiagonalLeft color 41 0 gs || newCheckDiagonalLeft color 40 0 gs ||
 newCheckDiagonalLeft color 39 0 gs ||
 newCheckDiagonalLeft color 38 0 gs = True
                             | otherwise = False
                             



windowDisplay :: Display
windowDisplay = InWindow "Window" (300, 300) (100, 100)

--main = animate windowDisplay white animationFunc

--animationFunc :: Float -> Picture
--animationFunc time = circleSolid (2*time)

type Model = (Float, Float)
data Slot = Empty | Red | Black 
type World = [(Float, Float)]
--world tar nu flera koordinater. kanske bör dom ta nycklar för sin pos
--world =/= gamestate ?? 
--play tar ingen gamestate??
{-}
main :: IO ()
main = play
  windowDisplay
  white
  20
  [(0, 0),(0,0)]
  drawingFunc
  inputHandler
  updateFunc

-}
mkCircle :: Color -> Float -> Float -> Picture
mkCircle col x y = pictures [translate x y $ color col $ circleSolid 26]

-- kolumn, jämna avstånd som kollar högre eller mindre från toppen, rutnät, ändrar x och y koordinater
drawingFunc :: World -> Picture
drawingFunc ((x, y):(z,q):xs) = pictures [rectangleSolid 1400 900, mkCircle white 0 40, mkCircle white 150 140, mkCircle white 300 240, mkCircle white 450 240, mkCircle white 300 140, mkCircle white 300 240, 
                                                                   mkCircle white 0 140, mkCircle white 0 240, mkCircle white 0 (-60), mkCircle white 150 240, mkCircle white 450 140,
                                                                   mkCircle white 0 (-160), mkCircle white 0 (-260), mkCircle white 150 40, mkCircle white 150 (-60), mkCircle white 150 (-160), mkCircle white 150 (-260),
                                                                   mkCircle white 300 40, mkCircle white 300 (-60), mkCircle white 300 (-160), mkCircle white 300 (-260),
                                                                   mkCircle white 450 40, mkCircle white 450 (-60), mkCircle white 450 (-160), mkCircle white 450 (-260),
                                                                   mkCircle white 600 240, mkCircle white 600 140, mkCircle white 600 40, mkCircle white 600 (-60), mkCircle white 600 (-160), mkCircle white 600 (-260),
                                                                   mkCircle white (-150) 240, mkCircle white (-150) 140, mkCircle white (-150) 40, mkCircle white (-150) (-60), mkCircle white (-150) (-160), mkCircle white (-150) (-260),
                                                                   mkCircle white (-300) 240, mkCircle white (-300) 140, mkCircle white (-300) 40, mkCircle white (-300) (-60), mkCircle white (-300) (-160), mkCircle white (-300) (-260)]

getcolor :: Int -> GameState -> Color
getcolor  index (x:xs) | index == 0 = snd x                  
                       | otherwise = (getcolor (index -1) xs)

getcolorcolor :: String -> GameState -> Color
getcolorcolor string gs | string == "Red" = red
                        | string == "Blue" = blue
                        | otherwise = white
ohmygoddrawingFunc :: GameState -> Picture
ohmygoddrawingFunc gs = pictures [rectangleSolid 1400 900, mkCircle (getcolor 1 gs ) 0 40, mkCircle (getcolor 2 gs) 150 140, mkCircle (getcolor 3 gs)  300 240, mkCircle (getcolor 4 gs ) 450 240, mkCircle (getcolor 5 gs ) 300 140, mkCircle (getcolor 6 gs ) 300 240, 
                                                           mkCircle (getcolor 7 gs ) 0 140, mkCircle (getcolor 8 gs ) 0 240, mkCircle (getcolor 9 gs ) 0 (-60), mkCircle (getcolor 10 gs ) 150 240, mkCircle (getcolor 11 gs ) 450 140,
                                                           mkCircle (getcolor 12 gs ) 0 (-160), mkCircle (getcolor 13 gs ) 0 (-260), mkCircle (getcolor 14 gs ) 150 40, mkCircle (getcolor 15 gs ) 150 (-60), mkCircle (getcolor 16 150 (-160), mkCircle (getcolor 17 gs ) 150 (-260),
                                                           mkCircle (getcolor 18 gs ) 300 40, mkCircle (getcolor 19 gs ) 300 (-60), mkCircle (getcolor 20 gs ) 300 (-160), mkCircle (getcolor 21 gs ) 300 (-260),
                                                           mkCircle (getcolor 22 gs ) 450 40, mkCircle (getcolor 23 gs ) 450 (-60), mkCircle (getcolor 24 gs ) 450 (-160), mkCircle (getcolor 25 gs ) 450 (-260),
                                                           mkCircle (getcolor 26 gs ) 600 240, mkCircle (getcolor 27 gs ) 600 140, mkCircle (getcolor 28 gs ) 600 40, mkCircle (getcolor 29 gs ) 600 (-60), mkCircle (getcolor 30 gs ) 600 (-160), mkCircle (getcolor 31 gs ) 600 (-260),
                                                           mkCircle (getcolor 32 gs ) (-150) 240, mkCircle (getcolor 33 gs ) (-150) 140, mkCircle (getcolor 34 gs ) (-150) 40, mkCircle (getcolor 35 gs )  (-150) (-60), mkCircle (getcolor 36 gs ) (-150) (-160), mkCircle (getcolor 37 gs ) (-150) (-260),
                                                           mkCircle (getcolor 38 gs ) (-300) 240, mkCircle (getcolor 39 gs ) (-300) 140, mkCircle (getcolor 40 gs ) (-300) 40, mkCircle (getcolor 41 gs ) (-300) (-60), mkCircle (getcolor 41 gs ) (-300) (-160), mkCircle (getcolor 41 gs ) (-300) (-260)]

inputHandler :: Event -> World -> World
inputHandler (EventKey (SpecialKey KeyUp) Down _ _) ((x, y):xs) = ((x, y - 10):xs)
inputHandler (EventKey (SpecialKey KeyDown) Down _ _) ((x, y):xs) = ((x, y + 10):xs)
inputHandler (EventKey (SpecialKey KeyRight) Down _ _) ((x, y):xs) = ((x + 10, y):xs)
inputHandler (EventKey (SpecialKey KeyLeft) Down _ _) ((x, y):xs) = ((x - 10, y):xs)
inputHandler (EventKey (Char  'w') Down _ _) ((x,y):(z,q):xs) = ((x,y):(z,q+10):xs)
inputHandler _ w = w

inputHandler2 :: Event -> World -> World
inputHandler2 (EventKey (SpecialKey KeyUp) Down _ _) ((x, y):xs) = ((x, y - 10):xs)
inputHandler2 (EventKey (SpecialKey KeyDown) Down _ _) ((x, y):xs) = ((x, y + 10):xs)
inputHandler2 (EventKey (SpecialKey KeyRight) Down _ _) ((x, y):xs) = ((x + 10, y):xs)
inputHandler2 (EventKey (SpecialKey KeyLeft) Down _ _) ((x, y):xs) = ((x - 10, y):xs)

inputHandler2 _ w = w

checkCoordinates :: Float -> Float
checkCoordinates x | x > 50 = 50
                   | otherwise = x

updateFunc :: Float -> World -> World
updateFunc _ ((x, y):xs) = ((towardCenter x, checkCoordinates $ towardCenter y):xs)
  where
    towardCenter :: Float -> Float
    towardCenter c = if abs c < 0.15
      then 0
      else if c > 0
        then c - 0.15
        else c + 0.15
