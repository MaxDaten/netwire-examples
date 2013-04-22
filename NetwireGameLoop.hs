{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
module NetwireGameLoop where

import Prelude hiding (id)
import Control.Monad.Reader
import Control.Wire
import Debug.Trace

data GameState = GameState

-- TODO: maybe a more complex monad
{-
newtype GameMonad a = GameMonad (ReaderT GameState IO a)
  deriving (Functor, Monad, MonadIO)
-}


newtype GameStateReader a = GameStateReader (Reader GameState a)
  deriving (Functor, Monad, MonadReader GameState)
type GameWire = WireM GameStateReader

runGameStateReader :: GameStateReader a -> GameState -> a
runGameStateReader (GameStateReader m) gs = runReader m gs


main :: IO ()
main = trace "starting" $ gameLoop gameWire gameSession
  where gameSession = clockSession

gameLoop :: (MonadIO m) => GameWire () Int ->  Session m -> m ()
gameLoop game s = do
  (dt, s') <- sessionUpdate s
  let (mx, w') = traceShow dt $ runGameStateReader (stepWire game dt ()) receiveGameState
  traceShow mx mx `seq` gameLoop w' s'


--
-- simple game wire concept:
-- input -> process -> output
-- input:   last gamestate, input (hid, mouse, network...)
-- process: game-object reactions (signals) (game logic), process graphics
-- output:  gamestate, render objects
--
-- arrow loop the gamestate <- system -< gamestate
gameWire :: GameWire a Int
gameWire = proc _ -> do
  frame <- countFrame -< ()
  x <- arr id -< traceShow frame
  returnA -< frame

receiveGameState :: GameState
receiveGameState = GameState

countFrame :: GameWire a Int
countFrame = countFrom 0 <<< 1