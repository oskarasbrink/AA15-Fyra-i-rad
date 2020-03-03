module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.Color
import Graphics
import Logic






window = InWindow "ConnectFour" (800, 600) (100, 100)
backgroundColor = makeColor 0 0 0 255

main :: IO ()
main =
   play windowDisplay black 5 ((1,(0,((-300),320))),(generateBoard')) drawingFunc inputHandler' (const id)


