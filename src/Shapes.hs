module Shapes where

data Colour = Black | Red | Green | Yellow | Blue | Magenta | Cyan | White
    deriving (Enum, Eq, Read, Show)

data Vector = Vector { getX :: Double, getY :: Double } deriving (Read, Show)

data Matrix = Matrix Vector Vector deriving (Read, Show)

data Shape = Empty | Circle | Square deriving (Read, Show)

data Transform =
      Identity
    | Translate Vector
    | Scale Vector
    | Compose Transform Transform
    | Rotate Matrix
    deriving (Read, Show)

data Style = Width Int | Fill Colour | Stroke Colour deriving Read

type Drawing = [(Transform, Shape, [Style])]

-- TODO: full range hex colours
colour :: Colour -> String
colour Black = "black"
colour Red   = "red"
colour Green = "green"
