{-# LANGUAGE OverloadedStrings #-}

module Server where

import qualified Data.List.Utils              as U
import           Data.Text.Lazy
import           Shapes                       (Drawing)
import           Svg                          (drawingToSvg)
import           Text.Blaze.Svg.Renderer.Text
import           Web.Scotty

start = scotty 3000 $
    get "/:input" $ do
        input <- U.replace "%20" " " <$> param "input"
        html $ mconcat ["<h1>Scotty, ", response input, " me up!</h1>"]

response :: String -> Text
response = renderSvg . drawingToSvg . read
