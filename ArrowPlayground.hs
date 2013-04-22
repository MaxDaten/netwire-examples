{-# LANGUAGE Arrows #-}
module ArrowPlayground where

import Control.Arrow
import Debug.Trace

--add :: Integral a => Arrow (a -> a)
incA :: (Arrow f, Num a) => f a a
incA = arr (+1) 

twelve :: Arrow f => f p Int
twelve = arr (const 12)

twelveInc :: Arrow f => f p Int
twelveInc = incA <<< twelve

tracingArr :: Arrow f => f a Int
tracingArr = proc _ -> do
	x <- twelveInc -< ()
	_ <- trace "xxx" id -< ()
	returnA -< x

main :: IO ()
main = do
  x <- runKleisli tracingArr id
  print x
