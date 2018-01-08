module Game (
    initialWorld,
    drawWorld, 
    handleEvents,
    simulateWorld
)where

import Collisions
import Types.General

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.Pure.Simulate
import Graphics.Gloss.Interface.Pure.Display



initialWorld :: ContexWorld
initialWorld = Play (Player (-270,0) (0, 0) (10, 70)) 
                    (Player (270,0) (0, 80) (10, 70))
                    (Ball (0,0) (100, 40) 15)
                    (Goal (-275, 0) 0 (10, 180))
                    (Goal (275, 0) 1 (10, 180))

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
                    $ w ]

drawWorld (Play (Player (px,py) (pvx,pvy) (pw, ph))
                (Player (pcx,pcy) (pcvx,pcvy) (pcw, pch))
                (Ball (x,y) (vx,vy) r) 
                (Goal (gx,gy) j (gw, gh))
                (Goal (gcx,gcy) jc (gcw, gch))) 
    = pictures [arena, goal, cgoal,  myPlayer,cPlayer ,ball]
    where 
        arena = color white (pictures [translate 0 0 (rectangleWire 550 550)])        
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

        updateBall :: Float -> Ball -> Ball
        updateBall dt (Ball (x, y) (dx, dy) r) = Ball (nx, ny) (ndx, ndy) r
            where 
                (nx, ndx) = clip x dx (maxX-r-15)
                (ny, ndy) = clip y dy (maxY-r-15)
                clip h dh max
                    | nh > max = (max , -dh)
                    | nh < -max = (-max , -dh)
                    | otherwise = (nh, dh)
                    where nh = h + dt * dh

        updatePlayer:: Float -> Player -> Player
        updatePlayer dt (Player (px, py) (pvx,pvy) (pw, ph)) = 
                Player (px, py') (pvx, pvy) (pw, ph)
                where
                    py' = py + dt * pvy
        
        
        updateComputer:: Float -> Ball -> Player -> Player
        updateComputer dt (Ball (bx, by) (bvx,bvy) r) (Player (pcx, pcy) (pcvx,pcvy) (pcw, pch) ) = 
            Player (pcx, pcy') (pcvx, pcvx') (pcw, pch)
            where      
                pcy' :: Float
                pcy'                     
                    | bvy >= 0 && by > pcy = pcy + dt * pcvy
                    | bvy < 0 && by < pcy = pcy + dt * pcvy
                    | otherwise = pcy
                    
                pcvx' :: Float
                pcvx' 
                    | bvy >= 0 = if pcvy > 0 then pcvy else -pcvy
                    | bvy < 0 = if pcvy < 0 then pcvy else -pcvy


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
handleEvents _ game = game 