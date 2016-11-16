module Shapes where

-- Example of a parsed `Drawing`.
_example = read "[(Identity, Circle, [FillColor \"#123456\"])]" :: Drawing

-- A `Colour` may be any string, hex or otherwise.
type Colour = String

data Vector = Vector { getX :: Double, getY :: Double } deriving (Read, Show)

data Matrix = Matrix Vector Vector deriving (Read, Show)

data Shape = Empty | Circle | Square deriving (Read, Show)

data Transform =
      Identity
    | Translate Vector
    | Scale     Vector
    | Compose   Transform Transform
    | Rotate    Matrix
    deriving (Read, Show)

data Style =
      Width       Int
    | Height      Int
    | FillColor   Colour
    | StrokeColor Colour
    | StrokeWidth Int
    deriving (Read, Show)

type Drawing = [(Transform, Shape, [Style])]
