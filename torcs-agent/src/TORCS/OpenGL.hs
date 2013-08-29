-- | 2d drawing utilities.
module TORCS.OpenGL where

import Data.IORef
import Graphics.Rendering.OpenGL hiding (Color, Point, Line, scale)
import Graphics.UI.GLUT hiding (Color, Line, Point, scale)

data Color = Color Double Double Double Double deriving Show

white, black, red, green, blue, yellow, cyan, magenta :: Color
white   = Color 1 1 1 0
black   = Color 0 0 0 0
red     = Color 1 0 0 0
green   = Color 0 1 0 0
blue    = Color 0 0 1 0
yellow  = Color 1 1 0 0
magenta = Color 1 0 1 0
cyan    = Color 0 1 1 0

light :: Color -> Color
light (Color r g b alpha) = Color r g b (alpha / 2)

data ShapeType = Point | Line | Rectangle deriving Show
data Shape = Shape ShapeType Color [(Double, Double)] deriving Show

type2mode :: ShapeType -> PrimitiveMode
type2mode Point = Points
type2mode Line = LineStrip
type2mode Rectangle = Polygon

class Drawable a where
   draw :: a -> IO ()

class Transformable a where
   rotate    :: Double -> a -> a
   scale     :: Double -> a -> a
   translate :: (Double,Double) -> a -> a
   mirrorV   :: a -> a
   mirrorH   :: a -> a

instance Drawable Shape where
   draw (Shape t (Color r g b a) ps) = renderPrimitive (type2mode t) $ do
      color $ Color4 (realToFrac r) (realToFrac g) (realToFrac b) ((realToFrac a) :: GLfloat)
      mapM_ (\(x,y) -> vertex $ Vertex3 (realToFrac x) (realToFrac y) (0::GLfloat)) ps

instance Transformable Shape where
   rotate rad (Shape t c ps) = Shape t c (map f ps)
      where f (x, y) = (x', y')
               where x' | sin rad == 0 = x * cos rad
                        | cos rad == 0 = - y * sin rad
                        | otherwise    = x * cos rad - y * sin rad
                     y' | sin rad == 0 = y * cos rad
                        | cos rad == 0 = x * sin rad
                        | otherwise    = x * sin rad + y * cos rad
   scale n (Shape t c ps) = Shape t c (map f ps)
      where f (x, y) = (x * n, y * n)
   translate (x', y') (Shape t c ps) = Shape t c (map f ps)
      where f (x, y) = (x + x', y + y')
   mirrorV (Shape t c ps) = Shape t c (map f ps)
      where f (x, y) = (x, -y)
   mirrorH (Shape t c ps) = Shape t c (map f ps)
      where f (x, y) = (-x, y)

replaceInfByOne :: [Shape] -> [Shape]
replaceInfByOne = map (\(Shape t c ps) -> Shape t c (map (replaceInfBy 1) ps))

replaceInfBy :: RealFloat a => a -> (a, a) -> (a, a)
replaceInfBy z (x', y') = (g x', g y')
   where g x | isInfinite x && x > 0 = z
             | isInfinite x && x < 0 = -z
             | otherwise             = x

mkLine :: Color -> (Double, Double) -> (Double, Double) -> Shape
mkLine c p1 p2 = Shape Line c [p1,p2]

mkLine' :: Color -> Double -> Double -> Shape
mkLine' c phi r = Shape Line c [(0,0), (x,y)]
   where x | cos phi == 0 = 0
           | otherwise    = r * cos phi
         y | sin phi == 0 = 0
           | otherwise    = r * sin phi

mkPolyline :: Color -> [(Double, Double)] -> Shape
mkPolyline c ps = Shape Line c ps

mkRect :: Color -> (Double, Double) -> (Double, Double) -> (Double, Double) -> (Double, Double) -> Shape
mkRect c p1 p2 p3 p4 = Shape Rectangle c [p1,p2,p3,p4]

mkRect' :: Color -> (Double, Double) -> (Double, Double) -> Shape
mkRect' c (x,y) (w,h) = Shape Rectangle c [(x-w/2,y+h/2), (x+w/2,y+h/2), (x+w/2,y-h/2), (x-w/2,y-h/2)]

runOpenGL :: IO [Shape] -> IO ()
runOpenGL getShapes = do
   _ <- getArgsAndInitialize
   _ <- createWindow "Golog Driver Visualization"
   scaleRef <- newIORef 1
   --translateRef <- newIORef (0,0)
   keyboardCallback $= Just (keyboard scaleRef)
   --mouseCallback $= Just (mouse scaleRef)
   mouseWheelCallback $= Just (mouseWheel scaleRef)
   reshapeCallback $= Just reshape
   displayCallback $= display getShapes scaleRef
   mainLoop

keyboard :: IORef Double -> Char -> Position -> IO ()
keyboard scaleRef '+' _ = modifyIORef' scaleRef (\x -> x+0.05)
keyboard scaleRef '-' _ = modifyIORef' scaleRef (\x -> x-0.05)
keyboard _        _   _ = return ()

--mouse :: IORef (Double, Double) -> MouseButton -> KeyState -> Position -> IO ()
--mouse translateRef LeftButton Down (Position x y) = 

mouseWheel :: IORef Double -> WheelNumber -> WheelDirection -> Position -> IO ()
mouseWheel scaleRef n d _ = do
   putStrLn $ "mouseWheel " ++ show n ++ " " ++ show d
   modifyIORef' scaleRef (\x -> x + (fromIntegral (d*n)) * 0.01)

reshape :: Size -> IO ()
reshape s@(Size _ _) = do
   viewport $= (Position 0 0, s)
   postRedisplay Nothing

display :: IO [Shape] -> IORef Double -> IO ()
display getShapes scaleRef = do
   shapes <- getShapes
   scaleFactor <- readIORef scaleRef
   let shapes' = map (scale scaleFactor) shapes
   clear [ColorBuffer]
   mapM_ draw shapes'
   flush
   addTimerCallback 10 (display getShapes scaleRef)

