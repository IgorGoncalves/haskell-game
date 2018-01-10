module Collisions
(
    collision
)where

import Types

collision :: ContexWorld -> ContexWorld
collision (Play (Player (px, py) (pvx,pvy) (pw, ph))
                (Player (pcx, pcy) (pcvx,pcvy) pcsize) 
                (Ball  (bx, by) (bvx, bvy) r)
                (Goal (gx, gy) j (gw, gh))
                (Goal (gcx, gcy) jc gcsize))


        |  PlayerCollision (Ball  (bx, by) (bvx, bvy) r) (Player (px, py) (pvx,pvy) (pw, ph)) ||
           PCPlayerCollision (Ball  (bx, by) (bvx, bvy) r) (Player (px, py) (pvx,pvy) (pw, ph)) 
                           (Player (pcx, pcy) (pcvx,pcvy) pcsize)                               = Play (Player (px, py) (pvx,pvy) (pw, ph) )
                                                                                                       (Player (pcx, pcy) (pcvx,pcvy) pcsize) 
                                                                                                       (Ball (nx, ny) (ndx, ndy) r) 
                                                                                                       (Goal (gx, gy) j (gw, gh))
                                                                                                       (Goal (gcx, gcy) jc gcsize)

        | gol (Ball (bx, by) (bvx, bvy) r) (Goal (gx, gy) j (gw, gh)) (Goal (gcx, gcy) jc gcsize) = GameOver (winner (Ball (bx, by) (bvx, bvy) r) 
                                                                                                               (Goal (gx, gy) j (gw, gh)) 
                                                                                                               (Goal (gcx, gcy) jc gcsize))
            
        | otherwise = Play  (Player (px, py) (pvx,pvy) (pw, ph) )
                            (Player (pcx, pcy) (pcvx,pcvy) pcsize)
                            (Ball (bx, by) (bvx, bvy) r)
                            (Goal (gx, gy) j (gw, gh))
                            (Goal (gcx, gcy) jc gcsize)
                                 
        where 
            (nx, ndx) = (bx, if bx > 0 then -bvx-pvy else -bvx+pvy)
            (ny, ndy) = (by, bvy-pvy)


PlayerCollision :: Ball -> Player -> Bool
PlayerCollision (Ball (bx, by) (bvx, bvy) r) (Player (px, py) (pvx,pvy) (pw, ph)) = bateu
            where
                bateu = bx <= (px + pw/2 + r) && 
                        by <= (py + ph/2) &&
                        by >= (py - ph/2) 
                    

PCPlayerCollision :: Ball -> Player -> Player -> Bool
PCPlayerCollision (Ball (bx, by) (bvx, bvy) r) (Player (px, py) (pvx,pvy) (pw, ph)) (Player (pcx, pcy) (pcvx,pcvy) pcsize)   = bateu
            where
                bateu = bx >= (pcx - pw/2 - r) && 
                        by <= (pcy + ph/2) &&
                        by >= (pcy - ph/2)
        
gol :: Ball -> Goal -> Goal -> Bool
gol (Ball (bx, by) (bvx, bvy) r) (Goal (gx, gy) j (gw, gh)) (Goal (gcx, gcy) jc gcsize) = golPl || golPC 
            where
                golPl = (bx <= (gx + r) && by + r <= (gy + gh/2) && by - r >= (gy - gh/2) )
                golPC = (bx + r >= gcx && by+r <= (gcy + gh/2) && by-r >= (gcy - gh/2))

        
winner :: Ball -> Goal -> Goal -> String
winner (Ball (bx, by) (bvx, bvy) r) (Goal (gx, gy) j (gw, gh)) (Goal (gcx, gcy) jc gcsize) = playerVencedor
            where
                playerVencedor = if bx <= (gx + r)
                                    then
                                        "Computer Win"
                                    else
                                        "Player Win"