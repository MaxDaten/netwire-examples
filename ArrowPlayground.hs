module ArrowPlayground where

import Control.Arrow

--add :: Integral a => Arrow (a -> a)
incA :: (Arrow f, Num a) => f a a
incA = arr (+1) 

twelve :: Arrow f => f p Int
twelve = arr (const 12)

twelveInc :: Arrow f => f p Int
twelveInc = incA <<< twelve

main :: IO ()
main = do
  x <- runKleisli twelveInc id
  print x
