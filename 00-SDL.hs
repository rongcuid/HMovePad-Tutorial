{-|
  This is the very first test to check whether SDL works
-}

module Main where

import qualified Graphics.UI.SDL as SDL

main :: IO ()
main = do
  SDL.init [SDL.InitEverything]
  w <- SDL.setVideoMode 800 600 32 [SDL.SWSurface]
  s <- SDL.createRGBSurfaceEndian [SDL.SWSurface] 800 600 32 
  SDL.fillRect s (Just testRect) (SDL.Pixel 0xFFFFFFFF)
  SDL.blitSurface s (Nothing) w (Nothing) 
  SDL.flip w
  testLoop
  SDL.quit
      where
        testLoop = testLoop
        testRect = SDL.Rect 350 500 100 50
