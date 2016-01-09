-- Code to generate projection models

import Graphics.Blank
import qualified Graphics.Blank.Style as Style
import Debug.Trace
import Control.Monad (when)
import Data.Text (pack, Text)

main = blankCanvas 3000 $ \ context -> do
  let (w,h) = (2508,1254) :: (Double,Double)
  let p :: Geographic -> (Double,Double)
      p (x,y) = {- traceShow (x,y,x',y')-} (((x' + 1) / 2) * w, ((-y' + 1) / 2) * h)
         where (x',y') = project (x,y)
  
  txt <- send context $ do
    top <- myCanvasContext
    c <- newCanvas (round w,round h)
    return ()
    with c $ do
      let longs = fmap (*30) [-6..6] :: [Int]
      let lats  = fmap (*30) [-3..3] :: [Int]
      grd <- createLinearGradient(0, 0, 0, h)
        -- light blue
      grd # addColorStop(0, "#8ED6FF")
      -- dark blue
      grd # addColorStop(1, "#004CB3")
      Style.fillStyle grd;
      fillRect(0,0,w,h)


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

      sequence_
          [ do  let (x,y) = p (fromIntegral long,fromIntegral lat)
--                () <- traceShow (long,lat,x,y) $ return ()
                beginPath()
                arc(x,y, 10, 0, 2 * pi, False)
                fillStyle "black"
                fill()
                font "16pt Calibri"
                fillText(pack (show long ++ "," ++ show lat) :: Text, x + 10, y + 20)
          | long <- longs
          , lat  <- lats
          ]

      -- now draw a cube 
      

      saveRestore $ do      
        beginPath()
        strokeStyle "black"
        lineWidth 5
        let line (x,y,z) (x',y',z') = 
              interpolate 50 (x,y,z) (x',y',z')  $ \ (x,y,z) (x',y',z') -> do
                  let (x1,y1) = three2Geographic (x,y,z)
                      (x2,y2) = three2Geographic (x',y',z')
                  () <- traceShow ((x1,y1),(x2,y2)) $ return ()
                  when (distance (p (x1,y1)) (p (x2,y2)) < 100) $ do
                    moveTo (p (x1,y1))
                    lineTo (p (x2,y2))
        let (x,y,z) = (1,1,1)
        line (x,y,z) (-x,y,z)     
        line (x,y,z) (x,-y,z)     
        line (x,y,z) (x,y,-z)     
        stroke()


      with top $ do
        drawImage (c,[0,0,width context,height context])

      toDataURL() -- of tempCanvas
  writeDataURL ("img.png") txt
  print "Done"
  
  


-- convert from our camera shell, in degrees, to the picture, normalized to 0..1.

-- https://en.wikipedia.org/wiki/Equirectangular_projection
-- (long,lat) 
-- input:
  -- long :: -180 .. 180
  -- lat  :: -90 ... 90
-- output:
  -- x :: -1 .. 1
  -- y :: -1 .. 1

type Longitude = Double -- -180 .. 180
type Latitude  = Double --  -90 .. 90
type Geographic = (Longitude,Latitude)
type X = Double         -- -1 .. 1, + on right
type Y = Double         -- -1 .. 1, + on top

project :: Geographic -> (X,Y)
project (x,y) = (x / 180,y / 90)
          
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


three2Geographic :: (Double,Double,Double) -> Geographic
three2Geographic (0,0,0) = (0,0)  -- choice
three2Geographic (x,y,z) = (radian2degree t,radian2degree u)
  where
      r = sqrt (x^2 + z^2)
      t = atan2 x (-z)
      u = atan (y / r)
      
radian2degree :: Double -> Double
radian2degree = (* (180 / pi))

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