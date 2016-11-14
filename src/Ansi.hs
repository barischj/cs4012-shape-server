module Ansi where

import Colours

clear :: IO ()
clear = putStr "\ESC[2J"

goto :: Int -> Int -> IO ()
goto x y = putStr $ concat ["\ESC[", show y, ";", show x, "H"]

color :: Colour -> String -> IO ()
color c s = putStr $ concat ["\ESC[3", show (fromEnum c), "m", s, "\ESC[0m"]
