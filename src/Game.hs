module Game (
    initialWorld,
    drawWorld,
    simulateWorld
)where

import Types
import Physics
import Collisions

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Simulate
import Graphics.Gloss.Interface.Pure.Display



initialWorld :: ContexWorld
initialWorld = Play (Player (-280,0) (0, 0) (10, 70)) 
                    (Player (282,0) (0, 80) (10, 70))
                    (Ball (0,0) (100, 40) 15)
                    (Goal (-281, 0) 0 (10, 180))
                    (Goal (280, 0) 1 (10, 180))

drawWorld :: ContexWorld -> Picture
drawWorld (GameOver w) = pictures [
                    scale 0.3 0.3 
                    . translate (-400) 0 
                    . color white 
                    . text 
                    $ "Game Over!",
                    scale 0.4 0.4 
                    . translate (-400) (-125)
                    . color green
                    . text 
                    $ w ,
                    scale 0.2 0.2 
                    . translate (-600) (-650)
                    . color white
                    . text 
                    $ "Press R to restart"]

drawWorld (Play (Player (px,py) (pvx,pvy) (pw, ph))
                (Player (pcx,pcy) (pcvx,pcvy) (pcw, pch))
                (Ball (x,y) (vx,vy) r) 
                (Goal (gx,gy) j (gw, gh))
                (Goal (gcx,gcy) jc (gcw, gch))) 
    = pictures [arena, goal, cgoal,  myPlayer,cPlayer ,ball]
    where 
        arena = color white (pictures [translate 0 0 (rectangleWire 570 570)])        
        myPlayer = color red (pictures [translate px py (rectangleSolid pw ph)])
        cPlayer = color red (pictures [translate pcx pcy (rectangleSolid pcw pch)])
        ball = color red (pictures [translate x y (circle r)])
        goal = color black (pictures [translate gx gy (rectangleSolid gw gh)])
        cgoal = color black (pictures [translate gcx gcy (rectangleSolid gcw gch)])

simulateWorld :: Float -> (ContexWorld -> ContexWorld)
simulateWorld _ (GameOver s) = GameOver s
simulateWorld timeStep (Play player cplayer ball goal cgoal) = nWorld
    where
        nWorld = collision (Play nPlayer ncPlayer nBall goal cgoal)
        nPlayer = updatePlayer timeStep player
        ncPlayer = updateComputer timeStep nBall cplayer
        nBall = updateBall timeStep ball
