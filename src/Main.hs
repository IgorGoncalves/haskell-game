module Main where    

import Game
import Graphics.Gloss
import Events

main :: IO ()
main =  play
            (InWindow "Drawing our initial world" (600, 600) (20,20))
            black 
            24
            initialWorld
            drawWorld 
            handleEvents
            simulateWorld
