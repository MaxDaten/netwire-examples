{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module NetwireGameLoop where

import Prelude hiding (id)
import Control.Monad.Reader
import Control.Wire

data GameState = GameState

-- TODO: maybe a more complex monad
{-
newtype GameMonad a = GameMonad (ReaderT GameState IO a)
  deriving (Functor, Monad, MonadIO)
-}

type GameStateReader = Reader GameState
type GameWire = WireM GameStateReader
  --deriving (Functor)

instance MonadIO GameStateReader where
  liftIO = undefined

main :: IO ()
main = gameLoop gameWire gameSession
  where gameSession = clockSession

gameLoop :: (MonadIO m) => GameWire () () ->  Session m -> IO ()
gameLoop game s = do
  --(dt, s') <- sessionUpdate s -- TODO: find a way with stepSession
  let   gs       = receiveGameState
        (mx, w', s') = runReader (stepSession game s ()) gs
  mx `seq` gameLoop w' s'


--
-- simple game wire concept:
-- input -> process -> output
-- input:   last gamestate, input (hid, mouse, network...)
-- process: game-object reactions (signals) (game logic), process graphics
-- output:  gamestate, render objects
--
-- arrow loop the gamestate <- system -< gamestate
gameWire :: GameWire a ()
gameWire = proc _ -> do
  frame <- countFrame -< ()
  returnA -< ()

receiveGameState :: GameState
receiveGameState = GameState

countFrame :: GameWire a Int
countFrame = countFrom 0 <<< 1