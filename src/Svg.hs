module Svg where

import           Data.Foldable               ()
import           Shapes
import qualified Text.Blaze.Internal         as I
import           Text.Blaze.Svg11            hiding (style)
import qualified Text.Blaze.Svg11.Attributes as A

-- |Useful for partially applying an argument to `!` in reverse order.
apply = flip (!)

-- |Applies an attribute to an `Svg`. The attribute is given as an
-- `AttributeValue -> Attribute` e.g. `A.height` and an `AttributeValue`
-- derived from an instance of `Show` e.g. `5`.
applyAttr :: Show a => (AttributeValue -> Attribute) -> a -> Svg -> Svg
applyAttr ava a = apply $ ava $ I.stringValue $ show a

-- |Applies a `Style` to an `Svg`.
styleSvg :: Style -> Svg -> Svg
styleSvg (FillColor   c) = applyAttr A.fill        c
styleSvg (Height      h) = applyAttr A.height      h
styleSvg (StrokeColor c) = applyAttr A.stroke      c
styleSvg (StrokeWidth w) = applyAttr A.strokeWidth w
styleSvg (Width       w) = applyAttr A.width       w

-- |Applies a `Transform` to an `Svg`.
transformSvg :: Transform -> Svg -> Svg
transformSvg Identity        = id
transformSvg (Translate v)   = apply $ A.transform $ translate (getX v) (getY v)
transformSvg (Scale v)       = apply $ A.transform $ scale     (getX v) (getY v)
transformSvg (Compose t1 t2) = transformSvg t2 . transformSvg t1
transformSvg (Rotate a)      = apply $ A.transform $ rotate a

-- |Converts a `Shape` to the corresponding `Svg`.
shapeToSvg :: Shape -> Svg
shapeToSvg Empty  = I.Empty
shapeToSvg Circle = circle
shapeToSvg Square = rect

-- |Converts a `Drawing` to the corresponding `Svg`.
drawingToSvg :: Drawing -> Svg
drawingToSvg [] = I.Empty
drawing ((transform, shape, styles):xs) =
    let shapeSvg    = shapeToSvg shape
        transformed = transformSvg transform shapeSvg
        styled      = foldr styleSvg transformed styles
    in styled `mappend` drawingToSvg xs
