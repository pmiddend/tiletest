module Main where

main :: IO ()
main = do
  wrenchPlay
    "window title"
    "media"
    Nothing
    (EngineState maybeImage (V2 0 0) (V2 0 0))
    30
    engineStateToPicture
    engineStateEventHandler
    engineStateTickHandler
