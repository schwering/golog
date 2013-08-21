-- | 2d drawing utilities.
module TORCS.OpenGL where

import Graphics.Rendering.OpenGL hiding (Color, Point, Line)
import Graphics.UI.GLUT hiding (Color, Line, Point)

data Color = Color Double Double Double Double

red, green, blue, yellow :: Color
red    = Color 1 0 0 0
green  = Color 0 1 0 0
blue   = Color 0 0 1 0
yellow = Color 1 1 0 0
white  = Color 1 1 1 0

light :: Color -> Color
light (Color r g b alpha) = Color r g b (alpha / 2)

data ShapeType = Point | Line | Rectangle
data Shape = Shape ShapeType Color [(Double, Double)]

type2mode :: ShapeType -> PrimitiveMode
type2mode Point = Points
type2mode Line = Lines
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
               where x' = x * cos rad - y * sin rad
                     y' = x * sin rad + y * cos rad
   scale n (Shape t c ps) = Shape t c (map f ps)
      where f (x, y) = (x * n, y * n)
   translate (x', y') (Shape t c ps) = Shape t c (map f ps)
      where f (x, y) = (x + x', y + y')
   mirrorV (Shape t c ps) = Shape t c (map f ps)
      where f (x, y) = (x, -y)
   mirrorH (Shape t c ps) = Shape t c (map f ps)
      where f (x, y) = (-x, y)

mkLine :: Color -> (Double, Double) -> (Double, Double) -> Shape
mkLine c p1 p2 = Shape Line c [p1,p2]

mkRect :: Color -> (Double, Double) -> (Double, Double) -> (Double, Double) -> (Double, Double) -> Shape
mkRect c p1 p2 p3 p4 = Shape Rectangle c [p1,p2,p3,p4]

mkRect' :: Color -> (Double, Double) -> (Double, Double) -> Shape
mkRect' c (x,y) (w,h) = Shape Rectangle c [(x-w/2,y+h/2), (x+w/2,y+h/2), (x+w/2,y-h/2), (x-w/2,y-h/2)]

runOpenGL :: IO [Shape] -> IO ()
runOpenGL getShapes = do
   _ <- getArgsAndInitialize
   _ <- createWindow "Golog Driver Visualization"
   reshapeCallback $= Just reshape
   displayCallback $= display getShapes
   mainLoop

reshape :: Size -> IO ()
reshape s@(Size _ _) = do
   viewport $= (Position 0 0, s)
   postRedisplay Nothing

display :: IO [Shape] -> IO ()
display getShapes = do
   shapes <- getShapes
   clear [ColorBuffer]
   mapM_ draw shapes
   flush
   addTimerCallback 10 (display getShapes)

