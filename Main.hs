module Main where
import Graphics.Gloss

backgroundColor = makeColor 255 255 255 255

initialGame = undefined

main :: IO ()
main = play backgroundColor 30 initialGame GameAsPicture TransformGame (\_ -> id)
