module Events where

import Types
import Game
import Graphics.Gloss.Interface.Pure.Game

handleEvents :: Event -> ContexWorld -> ContexWorld
handleEvents (EventKey (SpecialKey KeyUp) state _ _) 
                (Play (Player (px,py) (pvx,pvy) (pw, ph)) cplayer ball goal cgoal) = 
                Play (Player (px,py) (pvx, pvy') (pw, ph)) cplayer ball goal cgoal
                where
                    pvy' = if state == Down then 80 else 0
handleEvents (EventKey (SpecialKey KeyDown) state _ _) 
                (Play (Player (px,py) (pvx,pvy) (pw, ph)) cplayer ball goal cgoal) = 
                Play (Player (px,py) (pvx, pvy') (pw, ph)) cplayer ball goal cgoal
                where
                    pvy' = if state == Down then (-80) else 0
                    
handleEvents (EventKey (Char 'r') _ _ _) (GameOver _ ) = initialWorld
handleEvents (EventKey (Char 'r') _ _ _) Play {}  = initialWorld

handleEvents _ game = game 
