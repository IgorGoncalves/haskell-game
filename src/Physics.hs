module Physics where

import Collisions
import Types

updatePlayer:: Float -> Player -> Player
updatePlayer dt (Player (px, py) (pvx,pvy) (pw, ph)) = 
        Player (px, py') (pvx, pvy) (pw, ph)
        where
            py' = if((py + dt * pvy) < 250 && (py + dt * pvy) > -250 ) then py + dt * pvy else py

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
        
        
        
