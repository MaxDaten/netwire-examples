{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
module NetwireGameLoop where

import Prelude hiding (id, (.))
import Control.Monad.Reader
import Control.Wire
import Debug.Trace
import Text.Printf

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


gameLoop :: (Show b) => GameWire () b ->  Session IO -> IO ()
gameLoop game s = do
  (dt, s') <- sessionUpdate s
  let (mx, w') = runGameStateReader (stepWire game dt ()) receiveGameState
  either handleError draw mx
  gameLoop w' s'
  where
    handleError e = print $ "err:" ++ show e
    draw r = print $ "ok:" ++ show r


--
-- simple game wire concept:
-- input -> process -> output
-- input:   last gamestate, input (hid, mouse, network...)
-- process: game-object reactions (signals) (game logic), process graphics
-- output:  gamestate, render objects
-- arrow loop the gamestate <- system -< gamestate
gameWire :: GameWire a (Int, Double)
gameWire = proc _ -> do
  frameStat <- countFrame &&& avgFps 100 -< ()
  returnA -< traceShow (frameStat) frameStat


receiveGameState :: GameState
receiveGameState = GameState


countFrame :: (Monad m) => Wire e m a Int
countFrame = countFrom 0 <<< 1


-- maybe done with avgFps
avgFrameTime :: (Monad m) => Int -> Wire e m a Time
avgFrameTime n = avg n . dtime


timeString :: (Monad m) => Wire e m a String
timeString = fmap (printf "%8.2f") time


-- move to netwire contrib
traceW :: (Show b, Monad m) => Wire e m c b -> Wire e m c c
traceW w = (flip traceShow) id <$> w <*> id

