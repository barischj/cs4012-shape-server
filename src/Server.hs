{-# LANGUAGE OverloadedStrings #-}

module Server where

import qualified Data.List.Utils as U
import           Svg             (drawingToSvg, renderSvg)
import           Web.Scotty

start = scotty 3000 $
    get "/:input" $ do
        input <- U.replace "%20" " " <$> param "input"
        html $ renderSvg 500 500 $ drawingToSvg $ read input
