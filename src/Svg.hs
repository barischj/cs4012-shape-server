{-# LANGUAGE OverloadedStrings #-}

module Svg where

import           Data.Foldable               ()
import           Shapes
import qualified Text.Blaze.Internal         as I
import           Text.Blaze.Svg11            hiding (style)
import qualified Text.Blaze.Svg11.Attributes as A

-- |Useful for partially applying an argument to `!` in reverse order.
apply = flip (!)

-- |Applies a `Style` to an `Svg`. The `Style` is given as an
-- `AttributeValue -> Attribute` e.g. `A.height` and the `AttributeValue` is
-- derived from an instance of `Show` e.g. `5`.
applyStyle :: Show a => (AttributeValue -> Attribute) -> a -> Svg -> Svg
applyStyle ava a = apply $ ava $ I.stringValue $ show a

-- |Returns an `Svg` with a `Style` applied.
styleSvg :: Style -> Svg -> Svg
styleSvg (FillColor   c) = applyStyle A.fill        c
styleSvg (Height      h) = applyStyle A.height      h
styleSvg (StrokeColor c) = applyStyle A.stroke      c
styleSvg (StrokeWidth w) = applyStyle A.strokeWidth w
styleSvg (Width       w) = applyStyle A.width       w

-- |Returns an `Svg` with a `Transform` applied.
transformSvg :: Transform -> Svg -> Svg
transformSvg Identity        = id
transformSvg (Translate v)   = apply $ A.transform $ translate (getX v) (getY v)
transformSvg (Scale v)       = apply $ A.transform $ scale     (getX v) (getY v)
transformSvg (Compose t1 t2) = transformSvg t2 . transformSvg t1
transformSvg (Rotate a)      = apply $ A.transform $ rotate a

-- |Returns the `Svg` corresponding to a `Shape`.
shapeToSvg :: Shape -> Svg
shapeToSvg Empty  = I.Empty
shapeToSvg Circle = circle
shapeToSvg Square = rect

drawingToSvg :: Drawing -> Svg
drawingToSvg [] = I.Empty
drawing ((transform, shape, styles):xs) =
    let shapeSvg    = shapeToSvg shape
        transformed = transformSvg transform shapeSvg
        styled      = foldr styleSvg transformed styles
    in styled `mappend` drawingToSvg xs
