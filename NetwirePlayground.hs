{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}
module NetwireLoop where
-- https://danbst.wordpress.com/2013/01/23/novice-netwire-user/
-- http://jshaskell.blogspot.se/2012/11/breakout-improved-and-with-netwire.html
-- http://hpaste.org/83098


--import Control.Monad.Identity (Identity)
import Control.Wire
import Prelude hiding ((.), id)
import Text.Printf

countFrame :: WireP a Int
countFrame = countFrom 0 <<< 1

timestamp :: WireP a Time
timestamp = timeFrom 10

logFrame :: WireP (Int, Int) String
logFrame = arr (\(f1, f2) t -> printf "[%d %d] - %8.8f" f1 f2 t) <*> time

-- based on http://www.haskell.org/haskellwiki/Netwire
-- arrow do notation
system :: WireP a String
system = proc _ -> do
  --time  <- timestamp  -< ()
  frame <- countFrame -< ()
  --w <- countFrame . when even <|> 0 -< frame
  f2 <- hold 0 ((countFrom 0 <<< 1) . periodically 2) -< ()
  logFrame -< (frame, f2)

{-
-- count produces every 2
(countFrom 0 <<< 1) . periodically 2

-- count produces every instance, but periodically produces the count value only every 2
periodically 2 . (countFrom 0 <<< 1)
-}

{-
-- same as system
systemA :: WireP a String
systemA = timestamp &&& countFrame >>> arr (\ (t, f) -> printf "[%d] time: %s" f (show t))
-}

--
-- simple game wire:
-- input -> process -> output
-- input:   last gamestate, input (hid, mouse, network...)
-- process: game-object reactions (signals) (game logic), process graphics
-- output:  gamestate, render objects
--
-- arrow loop the gamestate <- system -< gamestate
main :: IO ()
main = mainloop system clockSession

mainloop w' session' = do
   (mx, w, session) <- stepSessionP w' session' ()
   case mx of
     Left ex -> putStrLn ("Inhibited: " ++ show ex)
     Right x -> putStrLn ("Produced: " ++ show x)
   mainloop w session
