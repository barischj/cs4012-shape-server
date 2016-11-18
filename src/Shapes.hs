module Shapes where

import qualified Data.Char as Ch

-- |Example of a parsed `Drawing`.
_example = read "[(Identity, Circle, [FillColor #123456])]" :: Drawing

-- |An RGB data type represented by three `Int`s.
-- The instance `Read RGB` ensures the `Int`s are in range.
data Rgb = Rgb Int Int Int

-- |Read an `RGB` value in the format #123456.
instance Read Rgb where
    readsPrec _ input = do
        ([h], r1:r2:g1:g2:b1:b2:rest) <- lex input
        [(Rgb (read [r1, r2]) (read [g1, g2]) (read [b1, b2]), rest)
         | h == '#' && all Ch.isDigit [r1, r2, g1, g2, b1, b2]]

-- |Show an `RGB` value in the format #123456.
instance Show Rgb where
    show (Rgb r g b) = "#" ++ concatMap show [r, g, b]

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
    | FillColor   Rgb
    | StrokeColor Rgb
    | StrokeWidth Int
    deriving (Read, Show)

-- |A drawing consists of `Shape`s, each with a `Transform` to apply and
-- possibly some `Style`s to apply.
type Drawing = [(Transform, Shape, [Style])]
