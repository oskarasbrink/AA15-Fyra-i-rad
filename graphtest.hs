
 --main = display (InWindow "Nice Window" (200, 200) (10, 10)) white (Circle 80)
 ---}

--ladda flera bilder
  --ladda i rätt ordning
  --"fokus" på nuvarande markör

--GAMEState för i fan

--fixa lista av bilder
  --skriv ny datatyp

--play-funktionen wtf 7 args

--hitbox markörer
  --spara markörers position
  
 --(mycke senare) vinstkontroll



import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss
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

main :: IO ()
main = play
  windowDisplay
  white
  20
  [(0, 0),(0,0)]
  drawingFunc
  inputHandler
  updateFunc


mkCircle :: Color -> Float -> Float -> Picture
mkCircle col x y = pictures [translate x y $ color col $ circleSolid 26]

drawingFunc :: World -> Picture
drawingFunc ((x, y):(z,q):xs) = pictures [rectangleSolid 1400 900, mkCircle white 0 40, mkCircle white 150 140, mkCircle white 300 240, mkCircle white 450 240, mkCircle white 300 140, mkCircle white 300 240, 
                                                                   mkCircle white 0 140, mkCircle white 0 240, mkCircle white 0 (-60), mkCircle white 150 240, mkCircle white 450 140,
                                                                   mkCircle white 0 (-160), mkCircle white 0 (-260), mkCircle white 150 40, mkCircle white 150 (-60), mkCircle white 150 (-160), mkCircle white 150 (-260),
                                                                   mkCircle white 300 40, mkCircle white 300 (-60), mkCircle white 300 (-160), mkCircle white 300 (-260),
                                                                   mkCircle white 450 40, mkCircle white 450 (-60), mkCircle white 450 (-160), mkCircle white 450 (-260),
                                                                   mkCircle white 600 240, mkCircle white 600 140, mkCircle white 600 40, mkCircle white 600 (-60), mkCircle white 600 (-160), mkCircle white 600 (-260),
                                                                   mkCircle white (-150) 240, mkCircle white (-150) 140, mkCircle white (-150) 40, mkCircle white (-150) (-60), mkCircle white (-150) (-160), mkCircle white (-150) (-260),
                                                                   mkCircle white (-300) 240, mkCircle white (-300) 140, mkCircle white (-300) 40, mkCircle white (-300) (-60), mkCircle white (-300) (-160), mkCircle white (-300) (-260)]
--[mkCircle black 20 40, mkCircle black 80 100, mkCircle black 140 160, mkCircle black 200 220]
--drawingFunc (x, y) = pictures[translate x y (Circle 50),translate x y (Circle 70)]
--rekursivt skriva ut allt till bilden wtf
--eller fylla lista med alla "möjliga" markörer. varje markör röd/blå/grå




  --loadpicture func
--picture where picture = picture <- loadBMP "hejhej.bmp"

--drawing :: Picture
--drawing = pictures
--  [ translate (-20) (-100) $ color ballColor $ circleSolid 30 (1)
--  , translate 30 50 $ color paddleColor $ rectangleSolid 10 50
--  ]
--  where
--    ballColor = dark red
--    paddleColor = light (light blue)

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


{-}

module Main(graphtest) where

import Graphics.Gloss

width, height, offset :: Int
width = 300
height = 300
offset = 100

window :: Display
window = InWindow "Pong" (width, height) (offset, offset)

background :: Color
background = black

main :: IO ()
main = display window background drawing 

circleSolid :: Float -> Picture
rectangleSolid :: Float -> Float -> Picture

drawing :: Picture
drawing = pictures
  [ circleSolid 30
  , rectangleSolid 10 50
  ]  where
    ballColor = dark red 
    paddleColor = light (light blue)
-}

{-}
width, height, offset :: Int
width = 300
height = 300
offset = 100

window :: Display
window = InWindow "Pong" (width, height) (offset, offset)

background :: Color
background = black

main :: IO ()
main = display window background drawing

rectangleSolid :: Float -> Float -> Picture

drawing :: Picture
drawing = pictures
  [rectangleSolid 10 50]


  -}