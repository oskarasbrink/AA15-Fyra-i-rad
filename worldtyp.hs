--main :: IO ()

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss
--type GameState = [((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String)]
type GameState = [((Float,Float),Color)]
type World = ((Int,(Int,(Float,Float))),GameState)
-- World bör skrivas om till en --Data Game osv


main =
   play windowDisplay black 5 ((1,(0,((-300),320))),(generateBoard')) drawingFunc inputHandler' (const id)
  

--inputHandler' :: Event -> World -> World

-- 3 första fallen väntar på knapptryck. Sista fallet (om inget knapptryck) skickar tillbaks samma värld
inputHandler' (EventKey (SpecialKey KeySpace) Down _ _) ((x,(index,t)),gs) = (((-1)*x,(index,t)),(newDropMannen (colorfunction x) 0 index gs))
inputHandler' (EventKey (SpecialKey KeyRight) Down _ _) ((x,t1),gs) = ((x,(plusArrowIndex t1)),gs)
inputHandler' (EventKey (SpecialKey KeyLeft) Down _ _) ((x,t1),gs) = ((x,(minusArrowIndex t1)),gs)
inputHandler' _ ((x,(index,t)),gs) = ((x,(index,t)),gs)

 -- flyttar pilen åt höger om den inte är längst till höger. tar pil-index och koordinat som argument. Mitt-delen av World-datatypen längst upp               
plusArrowIndex :: (Int,(Float,Float)) -> (Int,(Float,Float))
plusArrowIndex (index,(x,y)) | index == 7 = (7,(x,y))
                        | otherwise = ((index + 1),((x + 150 ),y))
--same but different
minusArrowIndex :: (Int,(Float,Float)) -> (Int,(Float,Float))
minusArrowIndex (index,(x,y)) | index == 1 = (1,(x,y))
                        | otherwise = ((index - 1),((x - 150) ,y))

-- spyr
ioStrSuger :: Color -> Int -> String -> GameState -> GameState
ioStrSuger color dim str gs = newDropMannen color dim (read str) gs

colorfunction :: Int -> Color 
colorfunction x | x == 1 = red
                | otherwise = blue

--spyr återigen, IO-krångel
playerMove' :: Color -> Int -> String -> GameState -> IO GameState
playerMove' color dim str gs = return (ioStrSuger color dim str gs)
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
--skippa denna
makegrey :: String -> [String]
makegrey g = [g,g,g,g,g,g,g
    , g,g,g,g,g,g,g
    , g,g,"Red",g,g,g,g
    , g,g,g,g,g,g,g
    , g,g,g,g,"Red",g,g
    , g,g,"Red","Red","Red","Red",g]

--vet faktiskt inte
newChangeColor :: Color -> Int -> GameState -> GameState
newChangeColor color x ((k,y):cs) | x > 0 = [(k,y)] ++ newChangeColor color (x-1) cs
                                  | otherwise = ((k,color):cs)                

-- skapar tom spelplan
generateBoard ::IO GameState
generateBoard = return [((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5))]

-- samma fast returnerar gamestate och INTE io-gamestate
generateBoard' :: GameState
generateBoard' = [((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5))]

-- Hämtar färgen från ett visst index
newTraverseList :: Int -> GameState -> Color
newTraverseList x (c:cs) | x == length (c:cs) = snd c
                         | x <= 0 = snd c
                         | otherwise = newTraverseList (x-1) cs


--droppar markören mannen. Kollar om något finns under. Obs här är gamestate och inte world
newDropMannen :: Color -> Int -> Int -> GameState -> GameState
newDropMannen color dim x gs | dim * 7 == 42 = gs
                             | (newTraverseList (((6-dim)*7)-(7-x)) gs) == greyN 0.5 = newChangeColor color (((6-dim)*7)-(7-x)) gs
                             | otherwise = newDropMannen  color (dim+1) x gs

--obs hanterar gamestate
newCheckWinColumn2 :: Color -> Int -> Int -> Int -> GameState -> Bool
newCheckWinColumn2 color row index tracker gs | tracker == 3 = True
                                              | index == 41 = False
                                              | index > 34 = newCheckWinColumn2 color (row +1) (row + 1) 0 gs
                                              | newTraverseList index gs == color = newCheckWinColumn2 color row (index + 7) (tracker +1) gs
                                              | otherwise = newCheckWinColumn2 color row (index + 7) tracker gs -- AKTA DIG


--vill jättegärna kunna hålla koll på koordinaten där markören hamnar 

--vill du verkligen det?


--obs hanterar gamestate och ej world
newCheckWinRow :: Color -> Int -> Int -> GameState -> Bool
newCheckWinRow color index tracker gs | tracker == 3 = True
                                      | index == length gs + 1 = False
                                      | index `mod` 7 == 6 = newCheckWinRow color (index + 1) 0 gs
                                      | newTraverseList (index) gs == color && newTraverseList (index +1 ) gs == color = (newCheckWinRow color (index + 1) (tracker +1) gs)
                                      | otherwise = newCheckWinRow color (index + 1) 0 gs
--obs hanterar gamestate och ej world
newCheckDiagonalRight :: Color -> Int -> Int -> GameState -> Bool
newCheckDiagonalRight color index tracker gs | tracker == 4 = True
                                             | index `mod`7 == 6 || index > 41 = False
                                             | newTraverseList index gs == color = newCheckDiagonalRight color (index -6) (tracker + 1) gs
                                             | otherwise = newCheckDiagonalRight color (index - 6) 0 gs
--obs hanterar gamestate och ej world
newCheckDiagonalLeft :: Color -> Int -> Int -> GameState -> Bool
newCheckDiagonalLeft color index tracker gs | tracker == 4 = True
                                            | index `mod` 7 == 0 || index < 0 = False
                                            | newTraverseList index gs == color = newCheckDiagonalLeft color (index - 8) (tracker +1 ) gs
                                            | otherwise = newCheckDiagonalLeft color (index - 8) 0 gs
--obs hanterar gamestate och ej world
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
                             


--Visa rutan
windowDisplay :: Display
windowDisplay = InWindow "Window" (300, 300) (100, 100)

--main = animate windowDisplay white animationFunc

--animationFunc :: Float -> Picture
--animationFunc time = circleSolid (2*time)

--this is shit
type Model = (Float, Float)
data Slot = Empty | Red | Black 

{-
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

-- hämtar ohmygod
getcolor :: Int -> GameState -> Color
getcolor  index (x:xs) | index == 0 = snd x                  
                       | otherwise = (getcolor (index -1) xs)



--playfunktionen
--skriva ut pilarna i animationsfunktionen
--fixa game datatypen "Game" istället för den kassa World

{-
ohmygoddrawingFunc gs = pictures [rectangleSolid 1400 900, mkCircle (getcolor 16 gs ) 0 40, mkCircle (getcolor 10 gs) 150 140, mkCircle (getcolor 4 gs)  300 240, mkCircle (getcolor 5 gs ) 450 240, mkCircle (getcolor 11 gs ) 300 140, 
                                                           mkCircle (getcolor 9 gs ) 0 140, mkCircle (getcolor 2 gs ) 0 240, mkCircle (getcolor 23 gs ) 0 (-60), mkCircle (getcolor 3 gs ) 150 240, mkCircle (getcolor 12 gs ) 450 140,
                                                           mkCircle (getcolor 30 gs ) 0 (-160), mkCircle (getcolor 37 gs ) 0 (-260), mkCircle (getcolor 17 gs ) 150 40, mkCircle (getcolor 24 gs ) 150 (-60), mkCircle (getcolor 31 gs) 150 (-160), mkCircle (getcolor 38 gs ) 150 (-260),
                                                           mkCircle (getcolor 18 gs ) 300 40, mkCircle (getcolor 25 gs ) 300 (-60), mkCircle (getcolor 32 gs ) 300 (-160), mkCircle (getcolor 39 gs ) 300 (-260),
                                                           mkCircle (getcolor 19 gs ) 450 40, mkCircle (getcolor 26 gs ) 450 (-60), mkCircle (getcolor 33 gs ) 450 (-160), mkCircle (getcolor 40 gs ) 450 (-260),
                                                           mkCircle (getcolor 6 gs ) 600 240, mkCircle (getcolor 13 gs ) 600 140, mkCircle (getcolor 20 gs ) 600 40, mkCircle (getcolor 27 gs ) 600 (-60), mkCircle (getcolor 34 gs ) 600 (-160), mkCircle (getcolor 41 gs ) 600 (-260),
                                                           mkCircle (getcolor 1 gs ) (-150) 240, mkCircle (getcolor 8 gs ) (-150) 140, mkCircle (getcolor 15 gs ) (-150) 40, mkCircle (getcolor 22 gs )  (-150) (-60), mkCircle (getcolor 29 gs ) (-150) (-160), mkCircle (getcolor 36 gs ) (-150) (-260),
                                                           mkCircle (getcolor 0 gs ) (-300) 240, mkCircle (getcolor 7 gs ) (-300) 140, mkCircle (getcolor 14 gs ) (-300) 40, mkCircle (getcolor 21 gs ) (-300) (-60), mkCircle (getcolor 28 gs ) (-300) (-160), mkCircle (getcolor 35 gs ) (-300) (-260)]
-}
--tar World som argument. Skriver ut allt som Picture
drawingFunc ((x,(index,t)),gs) = pictures [rectangleSolid 1400 900, mkCircle (getcolor 16 gs ) 0 40, mkCircle (getcolor 10 gs) 150 140, mkCircle (getcolor 4 gs)  300 240, mkCircle (getcolor 5 gs ) 450 240, mkCircle (getcolor 11 gs ) 300 140, 
                                                           mkCircle (getcolor 9 gs ) 0 140, mkCircle (getcolor 2 gs ) 0 240, mkCircle (getcolor 23 gs ) 0 (-60), mkCircle (getcolor 3 gs ) 150 240, mkCircle (getcolor 12 gs ) 450 140,
                                                           mkCircle (getcolor 30 gs ) 0 (-160), mkCircle (getcolor 37 gs ) 0 (-260), mkCircle (getcolor 17 gs ) 150 40, mkCircle (getcolor 24 gs ) 150 (-60), mkCircle (getcolor 31 gs) 150 (-160), mkCircle (getcolor 38 gs ) 150 (-260),
                                                           mkCircle (getcolor 18 gs ) 300 40, mkCircle (getcolor 25 gs ) 300 (-60), mkCircle (getcolor 32 gs ) 300 (-160), mkCircle (getcolor 39 gs ) 300 (-260),
                                                           mkCircle (getcolor 19 gs ) 450 40, mkCircle (getcolor 26 gs ) 450 (-60), mkCircle (getcolor 33 gs ) 450 (-160), mkCircle (getcolor 40 gs ) 450 (-260),
                                                           mkCircle (getcolor 6 gs ) 600 240, mkCircle (getcolor 13 gs ) 600 140, mkCircle (getcolor 20 gs ) 600 40, mkCircle (getcolor 27 gs ) 600 (-60), mkCircle (getcolor 34 gs ) 600 (-160), mkCircle (getcolor 41 gs ) 600 (-260),
                                                           mkCircle (getcolor 1 gs ) (-150) 240, mkCircle (getcolor 8 gs ) (-150) 140, mkCircle (getcolor 15 gs ) (-150) 40, mkCircle (getcolor 22 gs )  (-150) (-60), mkCircle (getcolor 29 gs ) (-150) (-160), mkCircle (getcolor 36 gs ) (-150) (-260),
                                                           mkCircle (getcolor 0 gs ) (-300) 240, mkCircle (getcolor 7 gs ) (-300) 140, mkCircle (getcolor 14 gs ) (-300) 40, mkCircle (getcolor 21 gs ) (-300) (-60), mkCircle (getcolor 28 gs ) (-300) (-160), mkCircle (getcolor 35 gs ) (-300) (-260)]


--används inte tror jag
checkCoordinates :: Float -> Float
checkCoordinates x | x > 50 = 50
                   | otherwise = x
