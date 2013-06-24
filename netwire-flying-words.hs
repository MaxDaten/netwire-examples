{-# LANGUAGE Arrows #-}

import Control.Wire
import Prelude hiding ((.), id)
import System.Console.ANSI
import Data.Maybe
import Control.Arrow
import Control.Monad.Trans.State
import Control.Monad.Trans
import Data.Time.Clock
import GHC.Conc

deltaClockSession dt =
    Session $ do
        t0 <- liftIO getCurrentTime
        return (0, loop t0)
    where
    loop t' =
        Session $ do
            threadDelay dt
            t <- liftIO getCurrentTime
            let realdt = realToFrac (diffUTCTime t t')
            return (realdt, loop t)

control whenInhibited whenProduced wire = loop wire (deltaClockSession 10000)
   where
     loop w' session' = do
         (mx, w, session) <- stepSession w' session' ()
         case mx of
           Left ex -> whenInhibited ex
           Right x -> whenProduced x
         loop w session `catch` (\_ ->  ansiFinishUI)

ansiFinishUI = do
    scrollPageUp 1
    setCursorPosition 24 0
    showCursor


impure f = mkFixM $ \_ x -> Right <$> f x
showW :: (Show a) => WireM IO a a
showW = impure (\x -> putStrLn (show x) >> return x )
clearScreenW = impure (\x -> clearScreen >> return x)
moveCursorW = impure (\(x, y) -> setCursorPosition y x >> return (x, y))

foreign import ccall unsafe "conio.h getch" c_getch :: IO Char
foreign import ccall unsafe "conio.h kbhit" c_kbhit :: IO Bool
whenKeyPressed = mkFixM $  \_ _ -> do 
    isKey <- c_kbhit
    if isKey then Right <$> c_getch
             else return (Left ())

redrawWordW word = 
    arr id &&& delay (40, 10)
    >>> impure redrawWord
   where redrawWord ((x, y), (prevx, prevy)) = do
            hideCursor
            setCursorPosition prevy prevx
            putStr (replicate (length word) ' ')
            setCursorPosition y x
            putStr word
            showCursor

drawUI =
    let horizontalLine = replicate 80 '='
    in do
        setCursorPosition 0 0
        putStr horizontalLine
        setCursorPosition 23 0
        putStr horizontalLine
        setCursorPosition 24 0
        putStr " > press 'q' to quit program"

ui = do clearScreen 
        hideCursor 
        drawUI 
        showCursor
startUI = perform . pure ui >>> inhibit ()

bounce (limX1, limY1)
       (limX2, limY2)
       ((x, y), (speedX, speedY)) ((dx, dy), _) =
  let newSx = if x >= limX2 || x <= limX1 
                then -speedX else speedX
      newSy = if y >= limY2 || y <= limY1 
                then -speedY else speedY
  in ((x + dx * newSx, y + dy * newSy), (newSx, newSy))

flyingWordW word startPos startVel = 
    periodically (1 / velocity) >>> pure 1
    >>> accum1 (bounce (1,1) (80 - length word, 22)) (startPos, startVel)
    >>> arr fst >>> redrawWordW word
    >>> pure ()
  where velocity = 20
  
quitBehavior =
    whenKeyPressed >>> when (== 'q') >>> quit
    
main = control return return $
    startUI
    -->
    flyingWordW "Love" (40, 10) (2, 1)
    &&& flyingWordW "Pain" (10, 5) (2, 1)
    &&& flyingWordW "Hate" (20, 10) (2, -1)
    &&& flyingWordW "Haskell" (20, 10) (-2, -2)
    &&& quitBehavior
    >>> pure ()