{-# LANGUAGE OverloadedStrings #-}

module Server where

import qualified Data.List.Utils as U
import           Data.Text.Lazy
import           Shapes          (Drawing)
import           Svg             (drawingToSvg, renderSvg)
import           Web.Scotty

start = scotty 3000 $
    get "/:input" $ do
        input <- U.replace "%20" " " <$> param "input"
        html $ response input

response :: String -> Text
response = renderSvg 500 500 . drawingToSvg . read
