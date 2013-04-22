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
	returnA -< traceShow x x

main :: IO ()
main = do
	m <- runKleisli tracingArr id
	m `seq` return ()
