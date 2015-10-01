# Foreword

After 3 months of digging through numerous websites and trying out some small projects, I finally get to implement a minimalistic game (or is it?), in a very, very different way. This example exists merely to demonstrate one possible structure of a game written in Haskell, and should easily be extended to handle more complex logic and gameplay.

# Abstract

This mini game has only one rectangle, which the player can move left and right by pressing Left and Right key, and that is the whole "game".

The game is implemented using `netwire-5.0.1`, with `SDL` handling graphics. If I understand correctly, the architecture is fully functional reactive. Almost everything is implemented by Arrow composition, with only one function exposed in `IO`. Therefore, I expect the reader to have basic understanding of the Arrow syntax of Haskell, since it is used extensively.

The implementation order of this game is chosen to make debugging easy, and the implementation itself is chosen to demonstrate different usage of `netwire` as much as possible.

Continuous time semantic is used for I/O, but discrete events are used to handle game events within the game logic.

# Set up SDL

The very first step is to make sure SDL works. The source is simple:

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

If everything works, there should be a white rectangle appearing on the bottom of the window appearing. Note that clicking the `x` will not close the window. It has to be closed by `Ctrl-C` or killing.


# Set up Output Wires

Since we do not want to implement all the way to the last step and find that nothing can be drawn on screen, we are doing the output part first.

We need the Arrows syntax:

    {-# LANGUAGE Arrows #-}

Also, we need to import some stuff:

    import Prelude hiding ((.), id)
    import Control.Wire
    import Control.Arrow
    import Control.Monad
    import Data.Monoid
    import qualified Graphics.UI.SDL as SDL


We need to understand how to construct Kleisli Wires: <http://stackoverflow.com/questions/32745934/kleisli-arrow-in-netwire-5>. A basic structure of a interactive program using Kleisli Wires is shown in this example: <http://stackoverflow.com/questions/30992299/console-interactivity-in-netwire>. To construct a Kleisli Wire from anything with type `a -> m b`, we need:

    mkKleisli :: (Monad m, Monoid e) => (a -> m b) -> Wire s e m a b
    mkKleisli f = mkGen_ $ \a -> liftM Right $ f a

Then, since I did not get `trace` to work under Arrow processes, a debug wire is made to print objects to console:

    wDebug :: (Show a, Monoid e) => Wire s e IO a ()
    wDebug = mkKleisli $ \a -> putStrLn $ show a

Now it is time to write some functions to be lifted into wires. For output, we need a function that returns a `SDL.Surface` with proper rectangle drawn given the X coordinate of the pad:

    padSurf :: SDL.Surface
                -> Int
                -> IO SDL.Surface
    padSurf surf x' = do
      let rect' = SDL.Rect x' 500 100 50
      clipRect <- SDL.getClipRect surf
      SDL.fillRect surf (Just clipRect) (SDL.Pixel 0x00000000)
      SDL.fillRect surf (Just rect') (SDL.Pixel 0xFFFFFFFF)
      return surf
    
Be careful, this function does destructive updates. The surface passed in will be blitted onto the window surface later.

Now we have the surface. The output wire is then trivil:

	wTestOutput :: SDL.Surface -> Wire s () IO () SDL.Surface
	wTestOutput surf = mkKleisli $ \_ -> testPad
	    where
	      testPad = padSurf surf 350

Then, we put wires together, and play with them a bit:

	gameWire :: SDL.Surface 
	         -> Wire s () IO () SDL.Surface
	gameWire w = proc _ -> do
	               finalSurf <- wTestOutput w -< ()
	               wDebug -< "Try a debug message"
	               returnA -< finalSurf

Finally, we change `main` and drive the wires properly:

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
	
Note that if you like, you can also make another wire to handle the main window surface too (and it is easy and better than my current implementation), but I was too late and lazy to add that. Check out the interactive example I mentioned above to see how simple `run` can get (it can get even simpler if inhibition is used instead of `quitWire` in that example).

When the program is run, its appearance should be the same as before.

Here is the complete code:

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
	
# Input Wires

In this section, we are going to construct wires that gets player input into the program.

Since we will use discrete events in the logic part, we need a data type for game events:

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
	
As comment suggested, the `Monoid` instance only applies for this particular game since it has only two opposite operations: left and right.

First, we will poll events from SDL:

	pollEvents :: [SDL.Event] -> IO (Either () ([SDL.Event]))
	pollEvents es = do
	  e <- SDL.pollEvent
	  case e of 
	    SDL.NoEvent -> return $ Right es
	    SDL.Quit -> return $ Left ()
	    _ -> pollEvents $ e:es

Obviously enough, this function polls events from SDL as a list, and inhibits when the `Quit` event is received.

Next, we need to check whether an event is a keyboard event:

	isKeyEvent :: SDL.Event -> Bool
	isKeyEvent (SDL.KeyDown k) = True
	isKeyEvent (SDL.KeyUp k) = True
	isKeyEvent _ = False

We will have a list of keys that are currently pressed, and it should update when a keyboard event occurs. In short, when a key is down, insert that key into the list, and vice versa:

	keyStatus :: [SDL.Keysym] -> [SDL.Event] -> [SDL.Keysym]
	keyStatus keysDown (e:es) = 
	    case e of
	      -- | If a KeyDown is detected, add key to list
	      SDL.KeyDown k -> keyStatus (k:keysDown) es
	      -- | If a KeyUp is detected, remove key from list
	      SDL.KeyUp k -> keyStatus (filter (/= k) keysDown) es
	      _ -> keyStatus keysDown es
    keyStatus keysDown [] = keysDown

Next, we write a function to convert a keyboard event to a game event:

	toGameEv :: SDL.Keysym -> GameEvent
	toGameEv (SDL.Keysym SDL.SDLK_RIGHT _ _) = MoveR
	toGameEv (SDL.Keysym SDL.SDLK_LEFT _ _) = MoveL
	toGameEv _ = NoEvent
	
We fold on the game events and get a single event (really, really, game specific!):

    fireGameEv :: [SDL.Keysym] -> GameEvent
    fireGameEv ks = foldl mappend NoEvent $ fmap toGameEv ks

Now we can start making wires.

First, we need a wire that polls events:

    wPollEvents :: Wire s () IO () [SDL.Event]
    wPollEvents = mkGen_ $ \_ -> pollEvents []

Note that `mkKleisli` makes wire that does not inhibit, but we want inhibition in this wire since the program should quit when it is supposed to. Therefore, we use `mkGen_` here.

Then, we need to filter the events. First, make a helper function that makes a continuous time filter wire:

    mkFW_ :: (Monad m, Monoid e) => (a -> Bool) -> Wire s e m [a] [a]
    mkFW_ f = mkSF_ $ filter f 

Use `mkFW_` to make a filter:

    wKeyEvents :: (Monad m, Monoid e) => Wire s e m [SDL.Event] [SDL.Event]
    wKeyEvents = mkFW_ isKeyEvent

Then, we need another convenient function to make a stateful wire from a stateful function of type `b -> a -> b`:

	mkSW_ :: (Monad m, Monoid e) => b -> (b->a->b) -> Wire s e m a b
	mkSW_ b0 f = mkSFN $ g b0
	    where
	      g b0 a = let b1 = f b0 a in 
	               (b1, mkSW_ b1 f)

Next, construct a stateful wire that remembers all key status:

    wKeyStatus :: (Monad m, Monoid e) => Wire s e m [SDL.Event] [SDL.Keysym]
    wKeyStatus = mkSW_ empty keyStatus

The last piece of wire segment fires the game event:

    wFireGameEv :: (Monad m, Monoid e) => Wire s e m [SDL.Keysym] (GameEvent)
    wFireGameEv = arr fireGameEv

To actively fire discrete events (netwire events) that contain game events, we need to hack netwire a bit (I think it is still quite incomplete) since it does not provide a wire that always fires events:

    always :: (Monad m, Monoid e) => Wire s e m a (Event a)
    always = mkSFN $ \x -> (WE.Event x, always)

Comparing to the implementation of `now`, the only difference is `never` and `always`.

Finally, a big wire that combines all input wires above:

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
	
An example of debugging is also shown in this wire.

To interface with the main program, modify `gameWire` to use the input:

	gameWire w = proc _ -> do
	               ev <- wGameInput -< ()
	               finalSurf <- wTestOutput w -< ()
	               returnA -< finalSurf
	
Nothing else needs to be changed. Well, interesting, isn't it?

When the program is run, the console gives a lot of output showing the current game events being fired. Try pressing left and right, and their combinations and see whether the behavior is expected. Of course, the rectangle will not move.

Here is a huge block of code:

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
	

# "Game" Logic --- Finally putting everything together!

First, we write an integrating function of the X position of the pad:

	padDX :: Int -> GameEvent -> Int
	padDX x0 e 
	    | x > 700 = 700
	    | x < 0 = 0
	    | otherwise = x
	    where
	      x = x0 + go e
	      go MoveR = dx
	      go MoveL = -dx
	      go _ = 0
	      dx = 15

I hard coded everything, but those are not important for this minimalistic example. It should be straightforward.

Then, we create the wire that represents the current position of the pad:

    wPadX :: (Monad m, Monoid e) => Wire s e m (Event GameEvent) Int
    wPadX = accumE padDX 400 >>> hold

`hold` holds at the latest value of a stream of discrete event.

Next, we put all logic things in a big logic wire:

	wGameLogic :: Wire s () IO (Event GameEvent) Int
	wGameLogic = proc ev -> do
	               x' <- wPadX -< ev
	               returnA -< x'
	
Since we have one state about the X coordinate, we need to modify the output wire:

	wGameOutput :: SDL.Surface -> Wire s () IO Int SDL.Surface
	wGameOutput surf = mkKleisli $ testPad
	    where
	      testPad = padSurf surf 
	
Finally, we chain everything in the `gameWire`:

	gameWire w = proc _ -> do
	               ev <- wGameInput -< ()
	               x <- wGameLogic -< ev
	               finalSurf <- wGameOutput w -< x
	               returnA -< finalSurf

Nothing needs to be changed in `main` and `run`. Wow!

And this is it! Run it and you shou be able to move the rectangle left and right!

A GIGANTIC block of code (I am curious how long will a C++ program that does the same thing be):

	{-|
      03-GameLogic.hs: The final product!
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
	
	-- | The integrator of X position of pad
	padDX :: Int -> GameEvent -> Int
	padDX x0 e 
	    | x > 700 = 700
	    | x < 0 = 0
	    | otherwise = x
	    where
	      x = x0 + go e
	      go MoveR = dx
	      go MoveL = -dx
	      go _ = 0
	      dx = 15
	
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
	               returnA -< e
	
	-- | The stateful wire of X position of pad
	wPadX :: (Monad m, Monoid e) => Wire s e m (Event GameEvent) Int
	wPadX = accumE padDX 400 >>> hold
	
	-- | This is the connected wire for the entire game logic
	wGameLogic :: Wire s () IO (Event GameEvent) Int
	wGameLogic = proc ev -> do
	               x' <- wPadX -< ev
	               returnA -< x'
	
	-- | The wire of output
	wGameOutput :: SDL.Surface -> Wire s () IO Int SDL.Surface
	wGameOutput surf = mkKleisli $ testPad
	    where
	      testPad = padSurf surf 
	
	
	-- | This is the main game wire
	gameWire :: SDL.Surface 
	         -- ^ The main surface (i.e. the window)
	         -> Wire s () IO () SDL.Surface
	gameWire w = proc _ -> do
	               ev <- wGameInput -< ()
	               x <- wGameLogic -< ev
	               finalSurf <- wGameOutput w -< x
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
	
