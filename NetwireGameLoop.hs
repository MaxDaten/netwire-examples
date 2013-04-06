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
main = gameLoop system clockSession

gameLoop :: GameWire () () ->  Session IO -> IO ()
gameLoop w s = do
  (dt, s) <- sessionUpdate s
  let (mx, w') = runReader (stepWire w dt ()) GameState
  mx `seq` gameLoop w' s

--system :: MyWire a b
system = undefined
