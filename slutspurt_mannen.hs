--main :: IO ()

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss
--type GameState = [((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String),((Float,Float),String)]
type GameState = [((Float,Float),Color)]
type World = [((Int,(Int,(Float,Float))),GameState)]

-- 0 == player 1 osv
main :: IO ()
main = do
   -- speltillstond <- generateBoard 
    putStrLn "blue player"
--    spela speltillstond 


--main =
--    play black 5 gameState (ohmygoddrawingFunc gameState) 

--display :: Display -> Color -> Picture -> IO
-- IO GameState

spela speltillstond = do
    putStrLn "blue coords:"
    bluecoords <- getLine
    newGameState <- playerMove' blue 0 bluecoords speltillstond
    display ( InWindow "Nice Window" (200, 200) (10, 10)) white (ohmygoddrawingFunc newGameState)
    putStrLn "red coords:"
    redcoords <- getLine
    newGameState <- playerMove' red 0 redcoords speltillstond
    display ( InWindow "Nice Window" (200, 200) (10, 10)) white (ohmygoddrawingFunc newGameState)
    
inputHandler' :: Event -> World -> World
inputHandler' (EventKey (SpecialKey KeySpace) Down _ _) (l:(x, y):xs) = newDropMannen 0 x gameState
inputHandler' (EventKey (SpecialKey KeyRight) Down _ _) (x:y:z) = (x:(plusArrowIndex x):z) 
--inputHandler' (EventKey (SpecialKey KeyLeft) Down _ _) (l:(x, y):xs) = ((x - 10, y):xs)
                
plusArrowIndex :: (Int,(Float,Float)) -> (Int,(Float,Float))
plusArrowIndex (index,(x,y)) gs | index == 7 = (7,(x,y))
                        | otherwise = ((index + 1),((x + 150 ),y))

minusArrowIndex :: (Int,(Float,Float)) -> (Int,(Float,Float))
minusArrowIndex (index,(x,y)) gs | index == 1 = (1,(x,y))
                        | otherwise = ((index - 1),((x - 150) ,y))

ioStrSuger :: Color -> Int -> String -> GameState -> GameState
ioStrSuger color dim str gs = newDropMannen color dim (read str) gs

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

makegrey :: String -> [String]
makegrey g = [g,g,g,g,g,g,g
    , g,g,g,g,g,g,g
    , g,g,"Red",g,g,g,g
    , g,g,g,g,g,g,g
    , g,g,g,g,"Red",g,g
    , g,g,"Red","Red","Red","Red",g]

newChangeColor :: Color -> Int -> GameState -> GameState
newChangeColor color x ((k,y):cs) | x > 0 = [(k,y)] ++ newChangeColor color (x-1) cs
                                  | otherwise = ((k,color):cs)                


generateBoard ::IO GameState
generateBoard = return [((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5))]


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


newTraverseList :: Int -> GameState -> Color
newTraverseList x (c:cs) | x == length (c:cs) = snd c
                         | x <= 0 = snd c
                         | otherwise = newTraverseList (x-1) cs



newDropMannen :: Color -> Int -> Int -> GameState -> GameState
newDropMannen color dim x gs | dim * 7 == 42 = gs
                          | (newTraverseList (((6-dim)*7)-(7-x)) gs) /= color = newChangeColor color (((6-dim)*7)-(7-x)) gs


newCheckWinColumn2 :: Color -> Int -> Int -> Int -> GameState -> Bool
newCheckWinColumn2 color row index tracker gs | tracker == 3 = True
                                              | index == 41 = False
                                              | index > 34 = newCheckWinColumn2 color (row +1) (row + 1) 0 gs
                                              | newTraverseList index gs == color = newCheckWinColumn2 color row (index + 7) (tracker +1) gs
                                              | otherwise = newCheckWinColumn2 color row (index + 7) tracker gs -- AKTA DIG


--vill jättegärna kunna hålla koll på koordinaten där markören hamnar 

newCheckWinRow :: Color -> Int -> Int -> GameState -> Bool
newCheckWinRow color index tracker gs | tracker == 3 = True
                                      | index == length gs + 1 = False
                                      | index `mod` 7 == 6 = newCheckWinRow color (index + 1) 0 gs
                                      | newTraverseList (index) gs == color && newTraverseList (index +1 ) gs == color = (newCheckWinRow color (index + 1) (tracker +1) gs)
                                      | otherwise = newCheckWinRow color (index + 1) 0 gs

newCheckDiagonalRight :: Color -> Int -> Int -> GameState -> Bool
newCheckDiagonalRight color index tracker gs | tracker == 4 = True
                                             | index `mod`7 == 6 || index > 41 = False
                                             | newTraverseList index gs == color = newCheckDiagonalRight color (index -6) (tracker + 1) gs
                                             | otherwise = newCheckDiagonalRight color (index - 6) 0 gs

newCheckDiagonalLeft :: Color -> Int -> Int -> GameState -> Bool
newCheckDiagonalLeft color index tracker gs | tracker == 4 = True
                                            | index `mod` 7 == 0 || index < 0 = False
                                            | newTraverseList index gs == color = newCheckDiagonalLeft color (index - 8) (tracker +1 ) gs
                                            | otherwise = newCheckDiagonalLeft color (index - 8) 0 gs

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
                             

mkSmallCircle :: Color -> Float -> Float -> Picture
mkSmallCircle col x y = pictures [translate x y $ color col $ circleSolid 13]

windowDisplay :: Display
windowDisplay = InWindow "Window" (300, 300) (100, 100)

--main = animate windowDisplay white animationFunc

--animationFunc :: Float -> Picture
--animationFunc time = circleSolid (2*time)

type Model = (Float, Float)
data Slot = Empty | Red | Black 
--type World = [(Float, Float)]
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

-- hämtar ohmygod
getcolor :: Int -> GameState -> Color
getcolor  index (x:xs) | index == 0 = snd x                  
                       | otherwise = (getcolor (index -1) xs)

--getcolorcolor :: String -> GameState -> Color
--getcolorcolor string gs | string == "Red" = red
--                        | string == "Blue" = blue
--                        | otherwise = white


ohmygoddrawingFunc2 gs = pictures [rectangleSolid 1400 900, mkCircle (getcolor 16 gs ) 0 40, mkCircle (getcolor 10 gs) 150 140, mkCircle (getcolor 4 gs)  300 240, mkCircle (getcolor 5 gs ) 450 240, mkCircle (getcolor 11 gs ) 300 140, 
                                                           mkCircle (getcolor 9 gs ) 0 140, mkCircle (getcolor 2 gs ) 0 240, mkCircle (getcolor 23 gs ) 0 (-60), mkCircle (getcolor 3 gs ) 150 240, mkCircle (getcolor 12 gs ) 450 140,
                                                           mkCircle (getcolor 30 gs ) 0 (-160), mkCircle (getcolor 37 gs ) 0 (-260), mkCircle (getcolor 17 gs ) 150 40, mkCircle (getcolor 24 gs ) 150 (-60), mkCircle (getcolor 31 gs) 150 (-160), mkCircle (getcolor 38 gs ) 150 (-260),
                                                           mkCircle (getcolor 18 gs ) 300 40, mkCircle (getcolor 25 gs ) 300 (-60), mkCircle (getcolor 32 gs ) 300 (-160), mkCircle (getcolor 39 gs ) 300 (-260),
                                                           mkCircle (getcolor 19 gs ) 450 40, mkCircle (getcolor 26 gs ) 450 (-60), mkCircle (getcolor 33 gs ) 450 (-160), mkCircle (getcolor 40 gs ) 450 (-260),
                                                           mkCircle (getcolor 6 gs ) 600 240, mkCircle (getcolor 13 gs ) 600 140, mkCircle (getcolor 20 gs ) 600 40, mkCircle (getcolor 27 gs ) 600 (-60), mkCircle (getcolor 34 gs ) 600 (-160), mkCircle (getcolor 41 gs ) 600 (-260),
                                                           mkCircle (getcolor 1 gs ) (-150) 240, mkCircle (getcolor 8 gs ) (-150) 140, mkCircle (getcolor 15 gs ) (-150) 40, mkCircle (getcolor 22 gs )  (-150) (-60), mkCircle (getcolor 29 gs ) (-150) (-160), mkCircle (getcolor 36 gs ) (-150) (-260),
                                                           mkCircle (getcolor 0 gs ) (-300) 240, mkCircle (getcolor 7 gs ) (-300) 140, mkCircle (getcolor 14 gs ) (-300) 40, mkCircle (getcolor 21 gs ) (-300) (-60), mkCircle (getcolor 28 gs ) (-300) (-160), mkCircle (getcolor 35 gs ) (-300) (-260)]


ohmygoddrawingFunc gs = pictures [rectangleSolid 1400 900, mkCircle (getcolor 16 gs ) 0 40, mkCircle (getcolor 10 gs) 150 140, mkCircle (getcolor 4 gs)  300 240, mkCircle (getcolor 5 gs ) 450 240, mkCircle (getcolor 11 gs ) 300 140, 
                                                           mkCircle (getcolor 9 gs ) 0 140, mkCircle (getcolor 2 gs ) 0 240, mkCircle (getcolor 23 gs ) 0 (-60), mkCircle (getcolor 3 gs ) 150 240, mkCircle (getcolor 12 gs ) 450 140,
                                                           mkCircle (getcolor 30 gs ) 0 (-160), mkCircle (getcolor 37 gs ) 0 (-260), mkCircle (getcolor 17 gs ) 150 40, mkCircle (getcolor 24 gs ) 150 (-60), mkCircle (getcolor 31 gs) 150 (-160), mkCircle (getcolor 38 gs ) 150 (-260),
                                                           mkCircle (getcolor 18 gs ) 300 40, mkCircle (getcolor 25 gs ) 300 (-60), mkCircle (getcolor 32 gs ) 300 (-160), mkCircle (getcolor 39 gs ) 300 (-260),
                                                           mkCircle (getcolor 19 gs ) 450 40, mkCircle (getcolor 26 gs ) 450 (-60), mkCircle (getcolor 33 gs ) 450 (-160), mkCircle (getcolor 40 gs ) 450 (-260),
                                                           mkCircle (getcolor 6 gs ) 600 240, mkCircle (getcolor 13 gs ) 600 140, mkCircle (getcolor 20 gs ) 600 40, mkCircle (getcolor 27 gs ) 600 (-60), mkCircle (getcolor 34 gs ) 600 (-160), mkCircle (getcolor 41 gs ) 600 (-260),
                                                           mkCircle (getcolor 1 gs ) (-150) 240, mkCircle (getcolor 8 gs ) (-150) 140, mkCircle (getcolor 15 gs ) (-150) 40, mkCircle (getcolor 22 gs )  (-150) (-60), mkCircle (getcolor 29 gs ) (-150) (-160), mkCircle (getcolor 36 gs ) (-150) (-260),
                                                           mkCircle (getcolor 0 gs ) (-300) 240, mkCircle (getcolor 7 gs ) (-300) 140, mkCircle (getcolor 14 gs ) (-300) 40, mkCircle (getcolor 21 gs ) (-300) (-60), mkCircle (getcolor 28 gs ) (-300) (-160), mkCircle (getcolor 35 gs ) (-300) (-260),
                                                           mkSmallCircle green (-300) 320]

{-}
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
-}
checkCoordinates :: Float -> Float
checkCoordinates x | x > 50 = 50
                   | otherwise = x

--pdateFunc :: Float -> World -> World
--updateFunc _ ((x, y):xs) = ((towardCenter x, checkCoordinates $ towardCenter y):xs)
--  where
--    towardCenter :: Float -> Float
--    towardCenter c = if abs c < 0.15
--      then 0
--      else if c > 0
--        then c - 0.15
--        else c + 0.15
