-- murdercopied from http://hpaste.org/83098
module Main where

import qualified Data.Set as S
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.Rendering.OpenGL as GL
import Control.Monad.Reader
import Control.Wire
import Data.Monoid
import Data.Set (Set)
import Data.VectorSpace
import Graphics.Rendering.OpenGL (($=))
import Prelude hiding ((.), id)


type AppWire = WireM (Reader (Set SDL.SDLKey))

data Scene =
    Scene {
      sceneRed    :: Bool,
      sceneCircle :: (Double, Double)
    }


appLoop :: AppWire () Scene -> Set SDL.SDLKey -> Session IO -> IO ()
appLoop w' keys' s' = do
    ev <- SDL.pollEvent
    case ev of
      SDL.KeyDown (SDL.Keysym key _ _) ->
          let keys = S.insert key keys'
              (mx, w) = runReader (stepWire w' 0 ()) keys
          in mx `seq` appLoop w keys s'

      SDL.KeyUp (SDL.Keysym key _ _) ->
          let keys = S.delete key keys'
              (mx, w) = runReader (stepWire w' 0 ()) keys
          in mx `seq` appLoop w keys s'

      SDL.NoEvent -> do
          (dt, s) <- sessionUpdate s'
          let (mx, w) = runReader (stepWire w' dt ()) keys'
          either (const (return ())) draw mx
          appLoop w keys' s

      SDL.Quit -> return ()
      _        -> appLoop w' keys' s'


draw :: Scene -> IO ()
draw sc = do
    GL.matrixMode $= GL.Projection
    GL.loadIdentity
    GL.ortho2D (-1) 1 (-1) 1

    GL.matrixMode $= GL.Modelview 0
    GL.loadIdentity

    let Scene { sceneRed = red,
                sceneCircle = (x, y)
              } = sc

    GL.clearColor $= GL.Color4 (if red then 1 else 0) 0 0 1
    GL.clear [GL.ColorBuffer]
    let vertex' x y = GL.vertex (GL.Vertex2 x (y :: GL.GLfloat))

    GL.color (GL.Color3 0 0.5 (0 :: GL.GLfloat))
    GL.renderPrimitive GL.TriangleFan $ do
        vertex' 0 0
        forM_ [0..64] $ \i ->
            let ang = pi*fromIntegral i / 32
            in vertex' (cos ang) (sin ang)

    GL.color (GL.Color3 1 1 (1 :: GL.GLfloat))
    GL.translate (GL.Vector3 (realToFrac x) (realToFrac y) (0 :: GL.GLfloat))
    GL.scale 0.1 0.1 (1 :: GL.GLfloat)
    let vertex' x y = GL.vertex (GL.Vertex2 x (y :: GL.GLfloat))
    GL.renderPrimitive GL.TriangleFan $ do
        vertex' 0 0
        forM_ [0..64] $ \i ->
            let ang = pi*fromIntegral i / 32
            in vertex' (cos ang) (sin ang)

    SDL.glSwapBuffers


keyPressed :: SDL.SDLKey -> AppWire a a
keyPressed key =
    mkFixM $ \_ x -> do
        isPressed <- asks (S.member key)
        return (if isPressed then Right x else Left mempty)


scene :: AppWire a Scene
scene = combine . circle
    where
    combine =
        Scene
        <$> (pure True . require ((> 1) . magnitude) <|> pure False)
        <*> id

    dirVeloc key dir = pure dir . keyPressed key <|> zeroV

    velocity =
        dirVeloc SDL.SDLK_w (0,  1) ^+^
        dirVeloc SDL.SDLK_s (0, -1) ^+^
        dirVeloc SDL.SDLK_a (-1, 0) ^+^
        dirVeloc SDL.SDLK_d ( 1, 0)

    circle = integral_ (0, 0) . velocity


main :: IO ()
main =
    SDL.withInit [SDL.InitVideo] $ do
        SDL.glSetAttribute SDL.glRedSize 8
        SDL.glSetAttribute SDL.glGreenSize 8
        SDL.glSetAttribute SDL.glBlueSize 8
        SDL.glSetAttribute SDL.glAlphaSize 0
        SDL.glSetAttribute SDL.glDoubleBuffer 0
        sf <- SDL.setVideoMode 640 640 32 [SDL.HWSurface, SDL.OpenGL, SDL.DoubleBuf]
        appLoop scene S.empty clockSession