{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
-- Code to generate projection models

import Graphics.Blank
import qualified Graphics.Blank.Style as Style
import Debug.Trace
import Control.Monad (when)
import Data.Text (pack, Text)
import Types

test :: Coordinate coord => coord -> (X,Y)
test = {- toCanvas .-} projectSphere . toSpherical
 where
  (w,h) = (2508,1254)   
  toCanvas (x',y') = (((x' + 1) / 2) * fromIntegral w, ((-y' + 1) / 2) * fromIntegral h)

main = blankCanvas 3000 $ \ context -> do

  let (w,h) = (2508,1254)   
  let toCanvas (x',y') = traceShow ("toCanvas",x',y') $ (((x' + 1) / 2) * w, ((-y' + 1) / 2) * h)
  let state = State ThreeSixty context (w,h)
                  (toCanvas . projectSphere . toSpherical)

  txt <- send context $ do
      img <- testCard state
      drawImage (img,[0,0,width context,height context])
      with img $ toDataURL()
  writeDataURL "test-sphere-360.png" txt

--  testCard context "test-dome-180.png"   (w,h) OneEighty  projectDome
  print "Done"
  
data Space = ThreeSixty | OneEighty

type X = Double         -- -1 .. 1, + on right
type Y = Double         -- -1 .. 1, + on top

data State = State
  { theSpace  :: Space
  , theDevice :: DeviceContext
  , theSize   :: (Double,Double)
  , screenProjection :: forall coord . Coordinate coord => coord -> (ScreenX,ScreenY)
  }

testCard :: State -> Canvas CanvasContext
testCard state = do
  let context = theDevice state
  let (w,h) = theSize state 
  let space = theSpace state
  let p :: Coordinate coord => coord -> (X,Y)
      p = screenProjection state  -- from the coord system, to the canvas location

  id $ do
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

      sequence_
         [ do beginPath()
              strokeStyle "red"
              lineWidth 2
              interpolate 10 (Geographic (long1,lat)) 
                             (Geographic (long2,lat)) $ \ p1 p2 -> do
                wrapLine state (p p1) (p p2)
              stroke()
         | (long1,long2) <- longs `zip` tail longs
         , lat  <- lats
         ]
      sequence_
         [ do beginPath()
              strokeStyle "green"
              lineWidth 2
              interpolate 10 (Geographic (long,lat1))
                             (Geographic (long,lat2)) $ \ p1 p2 -> do
                wrapLine state (p p1) (p p2)
              stroke()
         | long <- longs
         , (lat1,lat2) <- lats `zip` tail lats
         ]
{-
      sequence_
          [ do  let (x,y) = p $ Geographic (long,lat)
--                () <- traceShow (long,lat,x,y) $ return ()
                beginPath()
                wrapDot state (x,y) 10
                fillStyle "black"
                fill()
                font "16pt Calibri"
                when (long /= last longs) $ do
                  fillText(pack (show (round long) ++ "," ++ show (round lat)) :: Text, x + 10, y + 20)
          | long <- longs
          , lat  <- lats
          ]
-}
      -- now draw a cube 
{-
      saveRestore $ sequence_ [ do      
            beginPath()
            wrapDot state (p $ c) 10
            let (x,y) = p $ c
            () <- traceShow (c,x,y) $ return ()
            fillStyle "black"
            fill()
            strokeStyle "black"
            font "16pt Calibri"
            fillText(pack (show c) :: Text, x - 10, y - 20)
        | c <- [Cartesian (a,b,c)
               | a <- [-1,0,1]
               , b <- [-1,0,1]
               , c <- [-1,0,1]
               ] 

--        | c <- [Cartesian(0,0,0),Cartesian(1,0,1),Cartesian(1,1,1)]
        ]
-}
      saveRestore $ do      
        beginPath()
        strokeStyle "black"
        lineWidth 5
        cubeAt state (0,0,0) 4
        lineWidth 1
--        cubeAt state (-0.40,-0.40,-2) 0.25
        sequence [ cubeAt state (2, 0.4 * i, -0.40) 0.25
                 | i <- [-1,0,1]
                 ]
      return c
  


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

projectSphere :: Spherical -> (X,Y)
projectSphere (Spherical (_,t,u)) = (radian2degree (radian u) / 180,radian2degree (radian t) / 90)

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

     
cubeAt :: State -> (Double,Double,Double) -> Double -> Canvas ()
cubeAt state (xO,yO,zO) sz = do
  let (w,h) = theSize state 
  let space = theSpace state

  let p = screenProjection state  -- from the coord system, to the canvas location

  let guard z m = case space of
           ThreeSixty -> m
           OneEighty  -> if z <= 0 then m else return ()
    
  let fingers (x,y,z) = do
--        () <- traceShow ("fingers",x,y,z) $ return ()
        let wrap (x,y) = (if x < w/2 then x + w else x - w,y)
        let line (x,y,z) (x',y',z') = 
              interpolate 20 (x+xO,y+yO,z+zO) (x'+xO,y'+yO,z'+zO) $ \ (x,y,z) (x',y',z') -> guard z $ guard z' $ do
                  let p1 = p $ Cartesian (x,y,z)
                      p2 = p $ Cartesian (x',y',z')
--                  () <- traceShow ("interp",p1,p2) $ return ()
                  if (distance p1 p2 < w / 2)
                  then do moveTo p1
                          lineTo p2
                  else do moveTo p1
                          lineTo (wrap p2)
                          moveTo p2
                          lineTo (wrap p1)
        line (x,y,z) (-x,y,z)     
        line (x,y,z) (x,-y,z)     
        line (x,y,z) (x,y,-z)     

  let edge = sz / 2      
  fingers (edge,edge,edge)          
  fingers (-edge,-edge,edge)          
  fingers (edge,-edge,-edge)          
  fingers (-edge,edge,-edge)          
  stroke()

-- a line that crosses the edge of the screen
wrapLine :: State -> (X,Y) -> (X,Y) -> Canvas ()
wrapLine state p1 p2 = do
  let (w,h) = theSize state
  let wrap (x,y) = (if x < w/2 then x + w else x - w,y)
  if (distance p1 p2 < w / 2)
  then do moveTo p1
          lineTo p2
  else do moveTo p1
          lineTo (wrap p2)
          moveTo p2
          lineTo (wrap p1)

wrapDot :: State -> (X,Y) -> Double -> Canvas ()
wrapDot state (x,y) sz = do
  let (w,h) = theSize state
--  arc(x+w,y, sz, 0, 2 * pi, False)
  arc(x+0,y, sz, 0, 2 * pi, False)
--  arc(x-w,y, sz, 0, 2 * pi, False)  
