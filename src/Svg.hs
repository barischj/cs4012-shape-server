{-# LANGUAGE OverloadedStrings #-}

module Svg where

import Data.Foldable
import Shapes
import Text.Blaze.Svg11 hiding (style)
import qualified Text.Blaze.Internal as I
import qualified Text.Blaze.Svg11.Attributes as A

shapeToSvg :: Shape -> Svg
shapeToSvg Empty  = I.Empty
shapeToSvg Circle = circle
shapeToSvg Square = rect

{-
`!` has the type signature `Svg -> Attribute -> Svg`. `apply` is useful when
you want to partially apply an `Attribute` to `!` and return an `Svg -> Svg`.
-}
apply :: Attribute -> Svg -> Svg
apply attribute = flip (!) attribute

{-
Partially applies a `Style` to return an `Svg -> Svg`. The `Style` must be
given as an `AttributeValue -> Attribute` e.g. `A.width` and an instance of
`Show` e.g. `5`.
-}
applyStyle :: Show a => (AttributeValue -> Attribute) -> a -> Svg -> Svg
applyStyle ava a = apply $ ava $ I.stringValue $ show a

{-

-}
styleSvg :: Style -> Svg -> Svg
styleSvg (FillColor   c) = applyStyle A.fill        c
styleSvg (Height      h) = applyStyle A.height      h
styleSvg (StrokeColor c) = applyStyle A.stroke      c
styleSvg (StrokeWidth w) = applyStyle A.strokeWidth w
styleSvg (Width       w) = applyStyle A.width       w

transformSvg :: Transform -> Svg -> Svg
transformSvg Identity        = id
transformSvg (Translate v)   = apply $ A.transform $ translate (getX v) (getY v)
transformSvg (Scale v)       = apply $ A.transform $ scale     (getX v) (getY v)
transformSvg (Compose t1 t2) = transformSvg t2 . transformSvg t1

drawing :: Drawing -> Svg
drawing [] = I.Empty
drawing ((transform, shape, styles):xs) =
    let shapeSvg    = shapeToSvg shape
        transformed = transformSvg transform shapeSvg
        styled      = foldr ($) transformed (map styleSvg styles)
    in styled `mappend` drawing xs