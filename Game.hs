module Game where



{- ANVÄNDS INTE -}

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss

type GameState = [Color]
type Arrow = (Int,(Float,Float))
type Player = Int
type World = ((Player,Arrow),GameState)

            
generateBoard' :: GameState
generateBoard' = [(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5),(greyN 0.5)]



{-
import Data.Array

data CurrentState = GameOver (Maybe Player) | Running deriving (Eq, Show)
data Player = Player1 | Player2 deriving (Eq, Show)
data Slot = Taken Player | Empty deriving (Eq, Show)

type Board = Array (Int, Int) Slot

data Game = Game { gameBoard :: }

-}
