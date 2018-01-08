module Types.General where

data ContexWorld = Play Player Player Ball Goal Goal | GameOver String
    deriving (Eq,Show)

type Velocity     = (Float, Float)
type Size     = (Float, Float)
type PointInSpace = (Float, Float)

data Ball =  Ball PointInSpace Velocity Float
    deriving (Eq, Show)

data Player = Player PointInSpace Velocity Size
    deriving (Eq,Show)

data Goal = Goal PointInSpace Int Size
    deriving (Eq,Show)

maxX, maxY :: Float
maxX = 300
maxY = 300