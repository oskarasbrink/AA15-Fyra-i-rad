module Game where

import Data.Array

data CurrentState = GameOver (Maybe Player) | Running deriving (Eq, Show)
data Player = Player1 | Player2 deriving (Eq, Show)
data Slot = Taken Player | Empty deriving (Eq, Show)

type Board = Array (Int, Int) Slot

data Game = Game { gameBoard :: }