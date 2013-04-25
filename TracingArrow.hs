{-# LANGUAGE Arrows #-}

import Control.Arrow
import Control.Applicative
import Debug.Trace


-- i tried to build an identity arrow which traces an argument arrow
-- it doen't work, i guess the left side of (*>) isn't evaluated to the
-- needed depth to run the traceIO (guess²: only to the needed Applicative)
traceArr :: (Arrow a, Show b) => a c b -> a c c
traceArr a = unwrapArrow $ (flip traceShow) id <$> WrapArrow a <*> WrapArrow (arr id)

twelveArr :: Arrow f => f p Int
twelveArr = arr (const 12)

fortyTwoArr :: Arrow f => f p Int
fortyTwoArr = arr (const 42)

incArr :: (Arrow f, Num a) => f a a
incArr = arr (+1) 

exampleUse1 :: (Arrow f) => f p Int
exampleUse1 = traceArr fortyTwoArr <<< incArr <<< twelveArr -- expected a "42" trace

exampleUse2 :: (Arrow f) => f p Int
exampleUse2 = proc _ -> do
  twelve <- twelveArr -< ()
  thirty <- traceArr fortyTwoArr <<< incArr -< twelve -- expected a "42" trace
  returnA -< thirty


main :: IO ()
main = do
  e1 <- runKleisli exampleUse1 id
  print e1

  e2 <- runKleisli exampleUse2 id
  print e2
