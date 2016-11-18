module Main where

import           Shapes

a = reads "#123456__" :: [(Rgb, String)]
b = read "Width 5" :: Style
c = read "StrokeColor #123456" :: Style

main :: IO ()
main = print _example
