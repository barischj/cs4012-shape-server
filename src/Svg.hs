{-# LANGUAGE OverloadedStrings #-}

module Svg where

import Shapes
import Text.Blaze.Svg11
import qualified Text.Blaze.Internal as I
import qualified Text.Blaze.Svg11.Attributes as A

svg :: Shape -> Svg
svg Empty  = I.Empty
svg Circle = circle
svg Square = rect

apply :: Attribute -> Svg -> Svg
apply attribute = flip (!) attribute

-- data Style = Width Int | Fill Colour | Stroke Colour

style :: Style -> Svg -> Svg
style (Width w) = apply $ A.width $ I.stringValue $ show w

transform :: Transform -> Svg -> Svg
transform Identity        = id
transform (Translate v)   = apply $ A.transform $ translate (getX v) (getY v)
transform (Scale v)       = apply $ A.transform $ scale     (getX v) (getY v)
transform (Compose t1 t2) = transform t2 . transform t1


