{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}
module NetwireGameLoop where
-- http://hpaste.org/83098

import Prelude hiding (id)
import Control.Monad.Reader
import Control.Wire

data GameState = GameState

-- TODO: maybe a more complex monad
type GameMonad = Reader GameState
type GameWire = WireM GameMonad


main :: IO ()
main = gameLoop gameWire gameSession

gameLoop :: GameWire () () ->  Session IO -> IO ()
gameLoop game sh = do
  (dt, sh) <- sessionUpdate sh -- find a way with stepSession
  let (mx, w') = runReader (stepWire game dt ()) GameState
  mx `seq` gameLoop w' sh


--
-- simple game wire concept:
-- input -> process -> output
-- input:   last gamestate, input (hid, mouse, network...)
-- process: game-object reactions (signals) (game logic), process graphics
-- output:  gamestate, render objects
--
-- arrow loop the gamestate <- system -< gamestate
gameWire :: GameWire a b
gameWire = undefined

gameSession :: (MonadIO m) => Session m
gameSession = clockSession