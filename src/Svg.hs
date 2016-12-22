module Svg where

import           Data.Foldable                ()
import           Data.Text.Lazy               (Text)
import           Shapes
import qualified Text.Blaze.Internal          as I
import qualified Text.Blaze.Svg.Renderer.Text as R
import           Text.Blaze.Svg11             hiding (style)
import qualified Text.Blaze.Svg11.Attributes  as A

-- |Useful for partially applying an argument to `!` in reverse order.
apply = flip (!)

-- |Applies an attribute to an `Svg`. The attribute is given as an
-- `AttributeValue -> Attribute` e.g. `A.height` and an `AttributeValue`
-- derived from an instance of `Show` e.g. `5`.
applyShowAttr :: Show a => (AttributeValue -> Attribute) -> a -> Svg -> Svg
applyShowAttr ava a = apply $ ava $ I.stringValue $ show a

-- |Applies a `Style` to an `Svg`.
styleSvg :: Style -> Svg -> Svg
styleSvg (CX          cx) = applyShowAttr A.cx          cx
styleSvg (CY          cy) = applyShowAttr A.cy          cy
styleSvg (X           x)  = applyShowAttr A.x           x
styleSvg (Y           y)  = applyShowAttr A.y           y
styleSvg (R           r)  = applyShowAttr A.r           r
styleSvg (FillColor   c)  = applyShowAttr A.fill        c
styleSvg (Height      h)  = applyShowAttr A.height      h
styleSvg (StrokeColor c)  = applyShowAttr A.stroke      c
styleSvg (StrokeWidth w)  = applyShowAttr A.strokeWidth w
styleSvg (Width       w)  = applyShowAttr A.width       w

-- |Returns the corresponding `AttributeValue`s for a `Transform`.
tformAttrValue :: Transform -> [AttributeValue]
tformAttrValue Identity                 = []
tformAttrValue (Translate (Vector x y)) = [translate x y]
tformAttrValue (Scale     (Vector x y)) = [scale x y]
tformAttrValue (Compose t1 t2)          = concatMap tformAttrValue [t1, t2]
tformAttrValue (Rotate a)               = [rotate a]

-- |Applies a `Transform` to an `Svg`.
transformSvg :: Transform -> Svg -> Svg
transformSvg transform =
    apply $ A.transform $ mconcat $ tformAttrValue transform

-- |Converts a `Shape` to the corresponding `Svg`.
shapeToSvg :: Shape -> Svg
shapeToSvg Empty  = I.Empty
shapeToSvg Circle = circle
shapeToSvg Square = rect

-- |Converts a `Drawing` to the corresponding `Svg`.
drawingToSvg :: Drawing -> Svg
drawingToSvg [] = I.Empty
drawingToSvg ((transform, shape, styles):xs) =
    let shapeSvg       = shapeToSvg shape
        transformedSvg = transformSvg transform shapeSvg
        styledSvg      = foldr styleSvg transformedSvg styles
    -- `mappend` each SVG in the list together.
    in styledSvg `mappend` drawingToSvg xs

-- |Renders an `Svg` in valid HTML of given size in pixels.
renderSvg :: Int -> Int -> Svg -> Text
renderSvg w h svg_ = R.renderSvg $
    applyShowAttr A.height h $ applyShowAttr A.width w $ docTypeSvg svg_
