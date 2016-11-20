{-# LANGUAGE OverloadedStrings #-}

module Server where

import qualified Data.ByteString.Lazy.Char8 as Ch
import qualified Data.List.Utils            as U
import qualified Data.Text.Lazy             as T
import           Svg                        (drawingToSvg, renderSvg)
import           Web.Scotty

start = scotty 3000 $ do
    get "/" $ file "./src/example.html"
    post "/svg" $ do
        input <- body
        html $ renderSvg 500 500 $ drawingToSvg $ read $ Ch.unpack input

unpack :: Ch.ByteString -> String
unpack = Ch.unpack
