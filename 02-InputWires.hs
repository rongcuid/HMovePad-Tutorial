{-|
  02-InputWires.hs: This step, input wires are constructed and
  debugged by using wDebug
-}

{-# LANGUAGE Arrows #-}

module Main where

import Prelude hiding ((.), id)
import Control.Wire
import Control.Arrow
import Control.Monad
import Data.Monoid
import qualified Graphics.UI.SDL as SDL
import qualified Control.Wire.Unsafe.Event as WE

{- Data types -}
-- | The unified datatype of game events 
data GameEvent = MoveR
               | MoveL
               | NoEvent
                 deriving (Show, Eq)
-- | Make it Monoid so that game events can be combined 
-- (Only applicable in this "game"!)
instance Monoid GameEvent where
    mempty = NoEvent
    -- | Simultaneously moving left and right is just nothing
    MoveR `mappend` MoveL = NoEvent
    MoveL `mappend` MoveR = NoEvent
    -- | NoEvent is the identity
    NoEvent `mappend` x = x
    x `mappend` NoEvent = x
    x `mappend` y 
        -- | Make sure identical events return same events
        | x == y = x
        -- | Otherwise, no event
        | otherwise = NoEvent

{- Wire Utilities -}

-- | Make a stateless filter wire
mkFW_ :: (Monad m, Monoid e) => (a -> Bool) -> Wire s e m [a] [a]
mkFW_ f = mkSF_ $ filter f 

-- -- | Make a stateful wire from a chained stateful function and initial value
-- -- The function (a -> b -> a) takes in an old state /a/, and returns state 
-- -- transition function (b -> a). 
mkSW_ :: (Monad m, Monoid e) => b -> (b->a->b) -> Wire s e m a b
mkSW_ b0 f = mkSFN $ g b0
    where
      g b0 a = let b1 = f b0 a in 
               (b1, mkSW_ b1 f)

-- | Make a Kleisli wire
mkKleisli :: (Monad m, Monoid e) => (a -> m b) -> Wire s e m a b
mkKleisli f = mkGen_ $ \a -> liftM Right $ f a

-- | The debug wire
wDebug :: (Show a, Monoid e) => Wire s e IO a ()
wDebug = mkKleisli $ \a -> putStrLn $ show a

-- | The "always" wire
always :: (Monad m, Monoid e) => Wire s e m a (Event a)
always = mkSFN $ \x -> (WE.Event x, always)

{- Functions to be lifted -}

-- | This is the pad surface whose X coordinate can be updated
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


-- | The function to poll events and add to a list of events
pollEvents :: [SDL.Event] -> IO (Either () ([SDL.Event]))
pollEvents es = do
  e <- SDL.pollEvent
  case e of 
    SDL.NoEvent -> return $ Right es
    SDL.Quit -> return $ Left ()
    _ -> pollEvents $ e:es

-- | Checks whether one SDL.Event is a keyboard event
isKeyEvent :: SDL.Event -> Bool
isKeyEvent (SDL.KeyDown k) = True
isKeyEvent (SDL.KeyUp k) = True
isKeyEvent _ = False

-- | The raw function to process key status from events
keyStatus :: [SDL.Keysym] -> [SDL.Event] -> [SDL.Keysym]
keyStatus keysDown (e:es) = 
    case e of
      -- | If a KeyDown is detected, add key to list
      SDL.KeyDown k -> keyStatus (k:keysDown) es
      -- | If a KeyUp is detected, remove key from list
      SDL.KeyUp k -> keyStatus (filter (/= k) keysDown) es
      _ -> keyStatus keysDown es
-- | If all events are processed, return
keyStatus keysDown [] = keysDown

-- | Convert a SDL Keysym into "standard" game events
toGameEv :: SDL.Keysym -> GameEvent
toGameEv (SDL.Keysym SDL.SDLK_RIGHT _ _) = MoveR
toGameEv (SDL.Keysym SDL.SDLK_LEFT _ _) = MoveL
toGameEv _ = NoEvent

-- | Combine all game events to get one single firing
fireGameEv :: [SDL.Keysym] -> GameEvent
fireGameEv ks = foldl mappend NoEvent $ fmap toGameEv ks



{- Wires -}

-- | The Kleisli wire to poll events
wPollEvents :: Wire s () IO () [SDL.Event]
wPollEvents = mkGen_ $ \_ -> pollEvents []

-- | A stateless wire that filters out keyboard events
wKeyEvents :: (Monad m, Monoid e) => Wire s e m [SDL.Event] [SDL.Event]
wKeyEvents = mkFW_ isKeyEvent

-- | A stateful wire to keep track of key status
wKeyStatus :: (Monad m, Monoid e) => Wire s e m [SDL.Event] [SDL.Keysym]
wKeyStatus = mkSW_ empty keyStatus

-- | A wire to fire game events from SDL events
wFireGameEv :: (Monad m, Monoid e) => Wire s e m [SDL.Keysym] (GameEvent)
wFireGameEv = arr fireGameEv

-- | This is the connected wire for the entire game input
wGameInput :: Wire s () IO () (Event GameEvent)
wGameInput = proc _ -> do
               ge <- wFireGameEv <<< wKeyStatus
                     <<< wKeyEvents <<< wPollEvents -< ()
               e <- always -< ge
               -- Debug!
               case e of 
                 WE.NoEvent -> wDebug -< "No Event?!!"
                 WE.Event g -> wDebug -< "Game Event: " ++ show g
               -- End Debug
               returnA -< e

-- | The wire to test output
wTestOutput :: SDL.Surface -> Wire s () IO () SDL.Surface
wTestOutput surf = mkKleisli $ \_ -> testPad
    where
      testPad = padSurf surf 350


-- | This is the main game wire
gameWire :: SDL.Surface 
         -- ^ The main surface (i.e. the window)
         -> Wire s () IO () SDL.Surface
gameWire w = proc _ -> do
               ev <- wGameInput -< ()
               finalSurf <- wTestOutput w -< ()
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
