{-# LANGUAGE ScopedTypeVariables #-}
-- Code to generate projection models

import Graphics.Blank
import qualified Graphics.Blank.Style as Style
import Debug.Trace
import Control.Monad (when)
import Data.Text (pack, Text)
import Types

main = blankCanvas 3000 $ \ context -> do
  let (w,h) = (2508,1254) :: (Double,Double)
{-
  let p :: Geographic -> (Double,Double)
      p (x,y) = (((x' + 1) / 2) * w, ((-y' + 1) / 2) * h)
         where (x',y') = project (x,y)
-}
  testCard context "test-sphere-360.png" (w,h) ThreeSixty projectSphere
--  testCard context "test-dome-180.png"   (w,h) OneEighty  projectDome
  
data Space = ThreeSixty | OneEighty
{-
type Longitude = Double -- -180 .. 180
type Latitude  = Double --  -90 .. 90
type Geographic = (Longitude,Latitude)
type ScreenX = Double         -- -1 .. 1, + on right
type ScreenY = Double         -- -1 .. 1, + on top

type Distance    = Double     -- nominally meters
type Inclination = Double     -- 
type Azimuth     = Double

type Spherical = (Distance,Inclination,Azimuth)
type Cartesian = (Double,Double,Double)
-}

type X = Double         -- -1 .. 1, + on right
type Y = Double         -- -1 .. 1, + on top

data State = State
  { theSpace  :: Space
  , theDevice :: DeviceContext
  , theSize   :: (Int,Int)
  , screenProjection :: Geographic -> (ScreenX,ScreenY)
  }

testCard :: DeviceContext -> String -> (Double,Double) -> Space -> (Geographic -> (X,Y)) -> IO ()
testCard context fileName (w,h) space project = do

  let p :: Geographic -> (Double,Double)
      p (Geographic (x,y)) = (((x' + 1) / 2) * w, ((-y' + 1) / 2) * h)
         where (x',y') = project (Geographic (x,y))

  txt <- send context $ do
    top <- myCanvasContext
    c <- newCanvas (round w,round h)
    return ()
    with c $ do
      lineWidth 2

      let longs :: [Longitude]
                = fmap fromIntegral $ case space of
                    ThreeSixty -> fmap (*30) [-6..6] :: [Int]
                    OneEighty  -> fmap (*30) [-3..3] :: [Int]
      let lats :: [Latitude]
            = fmap fromIntegral (fmap (*30) [-3..3] :: [Int])
      grd <- createLinearGradient(0, 0, 0, h)
        -- light blue
      grd # addColorStop(0, "#8ED6FF")
      -- dark blue
      grd # addColorStop(1, "#004CB3")
      Style.fillStyle grd;
      fillRect(0,0,w,h)

{-
      sequence_
         [ do beginPath()
              strokeStyle "red"
              interpolate 10 (fromIntegral long1,fromIntegral lat) 
                             (fromIntegral long2,fromIntegral lat) $ \ (x1,y1) (x2,y2) -> do
                moveTo (p (x1,y1))
                lineTo (p (x2,y2))
              stroke()
         | (long1,long2) <- longs `zip` tail longs
         , lat  <- lats
         ]

      sequence_
         [ do beginPath()
              strokeStyle "green"
              interpolate 10 (fromIntegral long,fromIntegral lat1) 
                             (fromIntegral long,fromIntegral lat2) $ \ (x1,y1) (x2,y2) -> do
                moveTo (p (x1,y1))
                lineTo (p (x2,y2))
              stroke()
         | long <- longs
         , (lat1,lat2) <- lats `zip` tail lats
         ]
-}
      sequence_
          [ do  let (x,y) = p $ Geographic (long,lat)
--                () <- traceShow (long,lat,x,y) $ return ()
                beginPath()
                arc(x,y, 10, 0, 2 * pi, False)
                fillStyle "black"
                fill()
                font "16pt Calibri"
                fillText(pack (show (round long) ++ "," ++ show (round lat)) :: Text, x + 10, y + 20)
          | long <- longs
          , lat  <- lats
          ]
{-
      -- now draw a cube 
      

      saveRestore $ do      
        beginPath()
        strokeStyle "black"
        lineWidth 5
        cubeAt (w,h) space p (0,0,0) 400 
        lineWidth 1
        cubeAt (w,h) space p (-40,-40,-200) 25
        cubeAt (w,h) space p (0,-40,-200) 25
        cubeAt (w,h) space p (40,-40,-200) 25
        stroke()
-}
      with top $ do
        drawImage (c,[0,0,width context,height context])

      toDataURL() -- of tempCanvas
  writeDataURL fileName txt
  print $  "Written " ++ fileName
  
  


-- convert from our camera shell, in degrees, to the picture, normalized to 0..1.

-- https://en.wikipedia.org/wiki/Equirectangular_projection
-- (long,lat) 
-- input:
  -- long :: -180 .. 180
  -- lat  :: -90 ... 90
-- output:
  -- x :: -1 .. 1
  -- y :: -1 .. 1

{-
  type Longitude = Double -- -180 .. 180
type Latitude  = Double --  -90 .. 90
type Geographic = (Longitude,Latitude)
type X = Double         -- -1 .. 1, + on right
type Y = Double         -- -1 .. 1, + on top
-}

projectSphere :: Geographic -> (X,Y)
projectSphere (Geographic (x,y)) = (x / 180,y / 90)

projectDome :: Geographic -> (X,Y)
projectDome (Geographic (x,y)) = (x / 90,y / 90)
          
-- Plane
-- Sphere   -- 
-- Dome     -- 1/2 Sphere, looks okay
-- Full Dome ?          
-- Cube 

--    LFR
--    TBK

-- F = -60,-90 ... 60, 0
-- R = 60,-90 .. 180, 0
-- L = -180,-90 .. 60,0
-- BT = -60,0 .. 60,0 

-- K = 60,0 .. 180,90

-- Right handed
-- x = - on left, + on right
-- y = - below , + on top
-- z = - away, + closer


-- Not defined at (0,0,0)

{-
three2Geographic :: (Double,Double,Double) -> Geographic
three2Geographic (0,0,0) = (0,0)  -- choice
three2Geographic (x,y,z) = (radian2degree t,radian2degree u)
  where
      r = sqrt (x^2 + z^2)
      t = atan2 x (-z)
      u = atan (y / r)
-}

class Lerp a where
  lerp2 :: a -> a -> Double -> a

instance Lerp Double where
  lerp2 a b s = b * s + a * (1 - s)

instance (Lerp a, Lerp b) => Lerp (a,b) where
  lerp2 (a1,a2) (b1,b2) s = (lerp2 a1 b1 s,lerp2 a2 b2 s)
  
instance (Lerp a, Lerp b, Lerp c) => Lerp (a,b,c) where
  lerp2 (a1,a2,a3) (b1,b2,b3) s = (lerp2 a1 b1 s,lerp2 a2 b2 s,lerp2 a3 b3 s)  

interpolate :: (Monad m, Lerp a) => Int -> a -> a -> (a -> a -> m ()) -> m ()
interpolate n a b f = sequence_ [ f j j' | (j,j') <- js `zip` tail js ]
  where js = joints n a b
  
joints :: Lerp a => Int -> a -> a -> [a]
joints n a b = [ lerp2 a b (fromIntegral s/fromIntegral n) | s <- [0..n]]


-- wrap around if line is too long
distance :: (X,Y) -> (X,Y) -> Double
distance (x,y) (x',y') = sqrt (xd * xd + yd * yd)
   where
     xd = x - x'
     yd = y - y'
     
{-
cubeAt :: (Double,Double) -> Space -> (Geographic -> (Double,Double)) -> (Double,Double,Double) -> Double -> Canvas ()
cubeAt (w,h) space  p (xO,yO,zO) sz = do
    let guard z m = case space of
             ThreeSixty -> m
             OneEighty  -> if z <= 0 then m else return ()
      
    let fingers (x,y,z) = do
          let wrap (x,y) = (if x < w/2 then x + w else x - w,y)
          let line (x,y,z) (x',y',z') = 
                interpolate 25 (x+xO,y+yO,z+zO) (x'+xO,y'+yO,z'+zO) $ \ (x,y,z) (x',y',z') -> guard z $ guard z' $ do
                    let (x1,y1) = three2Geographic (x,y,z)
                        (x2,y2) = three2Geographic (x',y',z')
                    if (distance (p (x1,y1)) (p (x2,y2)) < w / 2)
                    then do moveTo (p (x1,y1))
                            lineTo (p (x2,y2))
                    else do () <- traceShow (p(x1,y1),p(x2,y2)) $ return ()
                            moveTo (p (x1,y1))
                            lineTo (wrap (p (x2,y2)))
                            moveTo (p (x2,y2))
                            lineTo (wrap (p (x1,y2)))
          line (x,y,z) (-x,y,z)     
          line (x,y,z) (x,-y,z)     
          line (x,y,z) (x,y,-z)     

    let edge = sz / 2      
    fingers (edge,edge,edge)          
    fingers (-edge,-edge,edge)          
    fingers (edge,-edge,-edge)          
    fingers (-edge,edge,-edge)          
    stroke()
     -}