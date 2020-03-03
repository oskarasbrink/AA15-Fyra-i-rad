
{-}
switchPlayer game =
    case gamePlayer game of
      PlayerX -> game { gamePlayer = PlayerO }
      PlayerO -> game { gamePlayer = PlayerX }


data Player = PlayerX | PlayerO deriving (Eq, Show)
data State = Running | GameOver (Maybe Player) deriving (Eq, Show)

type Board = [((Float,Float),Color)]

data Game = Game { gameBoard :: Board
                 , gamePlayer :: Player
                 , gameState :: State
                 } deriving (Eq, Show)
-}
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss
import Debug.Trace
type GameState = [Color]
type Arrow = (Int,(Float,Float))
type Player = Int
type World = ((Player,Arrow),GameState)
main =
   play windowDisplay black 5 ((1,(0,((-300),320))),(generateBoard')) drawingFunc inputHandler' (const id)


-- 3 första fallen väntar på knapptryck. Sista fallet (om inget knapptryck) skickar tillbaks samma värld

inputHandler' (EventKey (SpecialKey KeyEnter) Down _ _) ((_,(index,t)),gs) = ((1,(index,t)),generateBoard')
inputHandler' (EventKey (SpecialKey KeySpace) Down _ _) ((x,(index,t)),(p:xs)) | checkWin (colorFunction  x) (dropFunction (colorFunction x) 0 index (p:xs)) = (((-1)*x,((index,t))),(green:xs))
                                                                           | index < 7 && traverseList index (p:xs) /= (greyN 0.5) = ((x,(index,t)),(p:xs))
                                                                           | otherwise = (((-1)*x,(index,t)),(dropFunction (colorFunction x) 0 index (p:xs))) 
inputHandler' (EventKey (SpecialKey KeyRight) Down _ _) ((x,t1),gs) = ((x,(plusArrowIndex t1)),gs)
inputHandler' (EventKey (SpecialKey KeyLeft) Down _ _) ((x,t1),gs) = ((x,(minusArrowIndex t1)),gs)
inputHandler' _ ((x,(index,t)),gs) = ((x,(index,t)),gs)

 -- flyttar pilen åt höger om den inte är längst till höger. tar pil-index och koordinat som argument. Mitt-delen av World-datatypen längst upp               
plusArrowIndex :: Arrow -> Arrow
plusArrowIndex (index,(x,y)) | index == 6 = (6,(x,y))
                        | otherwise = ((index + 1),((x + 150 ),y))
--same but different
minusArrowIndex :: Arrow -> Arrow
minusArrowIndex (index,(x,y)) | index == 0 = (0,(x,y))
                        | otherwise = ((index - 1),((x - 150) ,y))

colorFunction :: Int -> Color 
colorFunction x | x == 1 = red
                | otherwise = blue


--vet faktiskt inte
changeColor :: Color -> Int -> GameState -> GameState
changeColor color x (c:cs) | x > 0 = [c] ++ changeColor color (x-1) cs
                                  | otherwise = (color:cs)                

-- samma fast returnerar gamestate och INTE io-gamestate
--generateBoard :: GameState
--generateBoard = [((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5)),((0,0),(greyN 0.5))]
generateBoard' :: GameState
generateBoard' = [(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5)]
-- Hämtar färgen från ett visst index
traverseList :: Int -> GameState -> Color
traverseList x (c:cs) | x == length (c:cs) = c
                         | cs == [] = c
                         | x <= 0 = c
                         | otherwise = traverseList (x-1) cs


--droppar markören mannen. Kollar om något finns under. Obs här är gamestate och inte world
dropFunction :: Color -> Int -> Int -> GameState -> GameState
dropFunction color dim x gs | dim * 7 == 42 =  gs
                             | (traverseList (((6-dim)*7)-(7-x)) gs) == greyN 0.5 = changeColor color (((6-dim)*7)-(7-x)) gs
                             | otherwise = dropFunction  color (dim+1) x gs

--obs hanterar gamestate
checkWinColumn :: Color -> Int -> Int -> Int -> GameState -> Bool
checkWinColumn color row index tracker gs | tracker == 3 = trace("asd") True
                                              | index == 41 = False
                                              | index > 34 = checkWinColumn color (row +1) (row + 1) 0 gs 
                                              | traverseList index gs == color && traverseList (index + 7) gs == color = checkWinColumn color row (index + 7) (tracker +1) gs
                                              | otherwise = checkWinColumn color row (index + 7) 0 gs -- AKTA DIG

--obs hanterar gamestate och ej world
checkWinRow :: Color -> Int -> Int -> GameState -> Bool
checkWinRow color index tracker gs | tracker == 3 = True
                                      | index == length gs + 1 = False
                                      | index `mod` 7 == 6 = checkWinRow color (index + 1) 0 gs
                                      | traverseList (index) gs == color && traverseList (index +1 ) gs == color = checkWinRow color (index + 1) (tracker +1) gs
                                      | otherwise = checkWinRow color (index + 1) 0 gs
--obs hanterar gamestate och ej world
checkDiagonalRight :: Color -> Int -> Int -> GameState -> Bool
checkDiagonalRight color index tracker gs | tracker == 3 = True
                                             | index `mod`7 == 6 || index < 0 = False
                                             | traverseList index gs == color && traverseList (index - 6) gs == color  = checkDiagonalRight color (index -6) (tracker + 1) gs
                                             | otherwise = checkDiagonalRight color (index - 6) 0 gs
--obs hanterar gamestate och ej world
checkDiagonalLeft :: Color -> Int -> Int -> GameState -> Bool
checkDiagonalLeft color index tracker gs | tracker == 3 = True
                                            | index `mod` 7 == 0 || index < 0 = False
                                            | traverseList index gs == color && traverseList (index - 8) gs == color = checkDiagonalLeft color (index - 8) (tracker +1 ) gs
                                            | otherwise = checkDiagonalLeft color (index - 8) 0 gs
--obs hanterar gamestate och ej world
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
                             
checkWin :: Color -> GameState -> Bool
checkWin color gs | checkDiagonal color gs || checkWinRow color 0 0 gs  || checkWinColumn color 0 0 0 gs = True
               | otherwise = False 

--Visa rutan
windowDisplay :: Display
windowDisplay = InWindow "Window" (800, 600) (100, 100)


mkSmallCircle :: Color -> Float -> Float -> Picture
mkSmallCircle col x y = pictures [translate x y $ color col $ circleSolid 13]

mkCircle :: Color -> Float -> Float -> Picture
mkCircle col x y = pictures [translate x y $ color col $ circleSolid 26]

getcolor :: Int -> GameState -> Color
getcolor  index (x:xs) | index == 0 = x                  
                       | otherwise = (getcolor (index -1) xs)

drawingFunc ((x,(index,(t1,t2))),gs) = pictures [rectangleSolid 1400 900, mkCircle (getcolor 16 gs ) 0 40, mkCircle (getcolor 10 gs) 150 140, mkCircle (getcolor 4 gs)  300 240, mkCircle (getcolor 5 gs ) 450 240, mkCircle (getcolor 11 gs ) 300 140, 
                                                           mkCircle (getcolor 9 gs ) 0 140, mkCircle (getcolor 2 gs ) 0 240, mkCircle (getcolor 23 gs ) 0 (-60), mkCircle (getcolor 3 gs ) 150 240, mkCircle (getcolor 12 gs ) 450 140,
                                                           mkCircle (getcolor 30 gs ) 0 (-160), mkCircle (getcolor 37 gs ) 0 (-260), mkCircle (getcolor 17 gs ) 150 40, mkCircle (getcolor 24 gs ) 150 (-60), mkCircle (getcolor 31 gs) 150 (-160), mkCircle (getcolor 38 gs ) 150 (-260),
                                                           mkCircle (getcolor 18 gs ) 300 40, mkCircle (getcolor 25 gs ) 300 (-60), mkCircle (getcolor 32 gs ) 300 (-160), mkCircle (getcolor 39 gs ) 300 (-260),
                                                           mkCircle (getcolor 19 gs ) 450 40, mkCircle (getcolor 26 gs ) 450 (-60), mkCircle (getcolor 33 gs ) 450 (-160), mkCircle (getcolor 40 gs ) 450 (-260),
                                                           mkCircle (getcolor 6 gs ) 600 240, mkCircle (getcolor 13 gs ) 600 140, mkCircle (getcolor 20 gs ) 600 40, mkCircle (getcolor 27 gs ) 600 (-60), mkCircle (getcolor 34 gs ) 600 (-160), mkCircle (getcolor 41 gs ) 600 (-260),
                                                           mkCircle (getcolor 1 gs ) (-150) 240, mkCircle (getcolor 8 gs ) (-150) 140, mkCircle (getcolor 15 gs ) (-150) 40, mkCircle (getcolor 22 gs )  (-150) (-60), mkCircle (getcolor 29 gs ) (-150) (-160), mkCircle (getcolor 36 gs ) (-150) (-260),
                                                           mkCircle (getcolor 0 gs ) (-300) 240, mkCircle (getcolor 7 gs ) (-300) 140, mkCircle (getcolor 14 gs ) (-300) 40, mkCircle (getcolor 21 gs ) (-300) (-60), mkCircle (getcolor 28 gs ) (-300) (-160), mkCircle (getcolor 35 gs ) (-300) (-260), mkSmallCircle (colorFunction x) t1 t2]

