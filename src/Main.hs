module Main where

import Control.Lens(from,(^.))
import Wrench.Engine(wrenchPlay,Picture(..),RenderPositionMode(..))
import Wrench.Angular
import Linear.V2
import Wrench.Color

main :: IO ()
main = do
  wrenchPlay
    "window title"
    (V2 640 480)
    "media"
    colorsWhite
    0
    1
    (const $ Translate (V2 100 100) $ Rotate (Degrees 90 ^. from degrees) $ Sprite "car" RenderPositionCenter)
    (\_ _ -> 0)
    (\_ _ -> 0)
