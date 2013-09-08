-- | 2d drawing utilities.
module TORCS.OpenGL
  (white, black, red, green, blue, yellow, cyan, magenta,
   Shape, draw, rotate, scale, translate, mirrorV, mirrorH,
   replaceInfByOne, replaceInfBy,
   mkLine, mkLine', mkPolyline, mkRect, mkRect',
   runOpenGL) where

import Data.IORef
import Graphics.Rendering.OpenGL hiding (RGB, scale, translate, rotate)
import Graphics.UI.GLUT hiding (RGB, scale, translate, rotate)

data RGB = RGB Double Double Double deriving Show

white, black, red, green, blue, yellow, cyan, magenta :: RGB
white   = RGB 1 1 1
black   = RGB 0 0 0
red     = RGB 1 0 0
green   = RGB 0 1 0
blue    = RGB 0 0 1
yellow  = RGB 1 1 0
magenta = RGB 1 0 1
cyan    = RGB 0 1 1

data Shape = Shape PrimitiveMode RGB [(Double, Double)] deriving Show

draw :: Shape -> IO ()
draw (Shape m (RGB r g b) ps) = renderPrimitive m $ do
   color $ Color4 (realToFrac r) (realToFrac g) (realToFrac b) (0 :: GLfloat)
   mapM_ (\(x,y) -> vertex $ Vertex3 (realToFrac x) (realToFrac y) (0::GLfloat)) ps

rotate :: Double -> Shape -> Shape
rotate rad (Shape m c ps) = Shape m c (map f ps)
   where f (x, y) = (x', y')
            where x' | sin rad == 0 = x * cos rad
                     | cos rad == 0 = - y * sin rad
                     | otherwise    = x * cos rad - y * sin rad
                  y' | sin rad == 0 = y * cos rad
                     | cos rad == 0 = x * sin rad
                     | otherwise    = x * sin rad + y * cos rad
scale :: Double -> Shape -> Shape
scale n (Shape m c ps) = Shape m c (map f ps)
   where f (x, y) = (x * n, y * n)

translate :: (Double, Double) -> Shape -> Shape
translate (x', y') (Shape m c ps) = Shape m c (map f ps)
   where f (x, y) = (x + x', y + y')

mirrorV :: Shape -> Shape
mirrorV (Shape m c ps) = Shape m c (map f ps)
   where f (x, y) = (x, -y)

mirrorH :: Shape -> Shape
mirrorH (Shape m c ps) = Shape m c (map f ps)
   where f (x, y) = (-x, y)

replaceInfByOne :: [Shape] -> [Shape]
replaceInfByOne = map (\(Shape m c ps) -> Shape m c (map (replaceInfBy 1) ps))

replaceInfBy :: RealFloat a => a -> (a, a) -> (a, a)
replaceInfBy z (x', y') = (g x', g y')
   where g x | isInfinite x && x > 0 = z
             | isInfinite x && x < 0 = -z
             | otherwise             = x

mkLine :: RGB -> (Double, Double) -> (Double, Double) -> Shape
mkLine c p1 p2 = Shape LineStrip c [p1,p2]

mkLine' :: RGB -> Double -> Double -> Shape
mkLine' c phi r = mkLine c (0,0) (x,y)
   where x | cos phi == 0 = 0
           | otherwise    = r * cos phi
         y | sin phi == 0 = 0
           | otherwise    = r * sin phi

mkPolyline :: RGB -> [(Double, Double)] -> Shape
mkPolyline c ps = Shape LineStrip c ps

mkRect :: RGB -> (Double, Double) -> (Double, Double) -> (Double, Double) -> (Double, Double) -> Shape
mkRect c p1 p2 p3 p4 = Shape Polygon c [p1,p2,p3,p4]

mkRect' :: RGB -> (Double, Double) -> (Double, Double) -> Shape
mkRect' c (x,y) (w,h) = Shape Polygon c [(x-w/2,y+h/2), (x+w/2,y+h/2), (x+w/2,y-h/2), (x-w/2,y-h/2)]

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

