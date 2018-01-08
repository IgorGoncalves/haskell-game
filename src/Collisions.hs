module Collisions
(
    collision
)where

import Types.General

collision :: ContexWorld -> ContexWorld
collision (Play (Player (px, py) (pvx,pvy) (pw, ph))
                (Player (pcx, pcy) (pcvx,pcvy) pcsize) 
                (Ball  (bx, by) (bvx, bvy) r)
                (Goal (gx, gy) j (gw, gh))
                (Goal (gcx, gcy) jc gcsize))
    |   bx <= (px + pw/2 + r) && 
        by <= (py + ph/2) &&
        by >= (py - ph/2) = Play (Player (px, py) (pvx,pvy) (pw, ph) )
                                (Player (pcx, pcy) (pcvx,pcvy) pcsize) 
                                (Ball (nx, ny) (ndx, ndy) r)
                                (Goal (gx, gy) j (gw, gh))
                                (Goal (gcx, gcy) jc gcsize)
    |   bx >= (pcx - pw/2 - r) && 
        by <= (pcy + ph/2) &&
        by >= (pcy - ph/2) = Play (Player (px, py) (pvx,pvy) (pw, ph) )
                                (Player (pcx, pcy) (pcvx,pcvy) pcsize) 
                                (Ball (nx, ny) (ndx, ndy) r)
                                (Goal (gx, gy) j (gw, gh))
                                (Goal (gcx, gcy) jc gcsize)
    |   bx <= (gx + r) && 
        by + r <= (gy + gh/2) && 
        by - r >= (gy - gh/2)  = GameOver "Computer Win"
    |   bx + r >= gcx && 
        by+r <= (gcy + gh/2) && 
        by-r >= (gcy - gh/2)  = GameOver "Player Win"
    
    | otherwise = Play  (Player (px, py) (pvx,pvy) (pw, ph) )
                        (Player (pcx, pcy) (pcvx,pcvy) pcsize)
                        (Ball (bx, by) (bvx, bvy) r) 
                        (Goal (gx, gy) j (gw, gh))
                        (Goal (gcx, gcy) jc gcsize)
    where 
        (nx, ndx) = (bx, if bx > 0 then -bvx-pvy else -bvx+pvy)
        (ny, ndy) = (by, bvy-pvy)