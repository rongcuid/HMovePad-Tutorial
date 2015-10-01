{-|
  01-OutputWires.hs: This step, the output wires are constructed first for
  easy debugging
-}

{-# LANGUAGE Arrows #-}

module Main where

import Prelude hiding ((.), id)
import Control.Wire
import Control.Arrow
import Control.Monad
import Data.Monoid
import qualified Graphics.UI.SDL as SDL

{- Wire Utilities -}

-- | Make a Kleisli wire
mkKleisli :: (Monad m, Monoid e) => (a -> m b) -> Wire s e m a b
mkKleisli f = mkGen_ $ \a -> liftM Right $ f a

-- | The debug wire
wDebug :: (Show a, Monoid e) => Wire s e IO a ()
wDebug = mkKleisli $ \a -> putStrLn $ show a

{- Functions to be lifted -}

padSurf :: SDL.Surface
            -- ^ Previous state of surface 
            -> Int
            -- ^ X'
            -- | New state
            -> IO SDL.Surface
padSurf surf x' = do
  let rect' = SDL.Rect x' 500 100 50
  clipRect <- SDL.getClipRect surf
  SDL.fillRect surf (Just clipRect) (SDL.Pixel 0x00000000)
  SDL.fillRect surf (Just rect') (SDL.Pixel 0xFFFFFFFF)
  return surf


{- Wires -}

wTestOutput :: SDL.Surface -> Wire s () IO () SDL.Surface
wTestOutput surf = mkKleisli $ \_ -> testPad
    where
      testPad = padSurf surf 350


-- | This is the main game wire
gameWire :: SDL.Surface 
         -- ^ The main surface (i.e. the window)
         -> Wire s () IO () SDL.Surface
gameWire w = proc _ -> do
               finalSurf <- wTestOutput w -< ()
               wDebug -< "Try a debug message"
               returnA -< finalSurf

main :: IO ()
main = do
  SDL.init [SDL.InitEverything]
  w <- SDL.setVideoMode 800 600 32 [SDL.SWSurface]
  s <- SDL.createRGBSurfaceEndian [SDL.SWSurface] 800 600 32 
  run w (countSession_ 1) $ gameWire w
  SDL.quit

run ::SDL.Surface ->  Session IO s -> Wire s () IO () SDL.Surface -> IO ()
run mainSurf s w  = do
  (ds, s') <- stepSession s
  (eSrcSurf, w') <- stepWire w ds (Right ())
  case eSrcSurf of 
    Right srcSurf -> do 
                  SDL.blitSurface srcSurf (Nothing) mainSurf (Nothing)
                  SDL.flip mainSurf
                  SDL.delay 30
                  run mainSurf s' w'
    _ -> return ()
