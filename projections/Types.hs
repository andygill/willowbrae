{-# LANGUAGE ScopedTypeVariables #-}
module Types where

import Debug.Trace
import Test.QuickCheck

type Longitude = Double -- -180 .. 180
type Latitude  = Double --  -90 .. 90
newtype Geographic = Geographic (Longitude,Latitude)
  deriving Show

type ScreenX = Double         -- -1 .. 1, + on right
type ScreenY = Double         -- -1 .. 1, + on top

type Distance    = Double     -- nominally meters
type Inclination = Double     -- "latitude", In Radians, 0 is ahead, + is up, normalized to +/- pi/2
type Azimuth     = Double     -- "longiture", In Radians, 0 is ahead, + is right, normalized to +/- pi

newtype Spherical = Spherical (Distance,Inclination,Azimuth)
  deriving Show

-- x == in/out, in+
-- y == left/right, right+
-- z == up/down, up+

newtype Cartesian = Cartesian (Double,Double,Double)         -- in nominal meters
  deriving Show

newtype ScreenCoord = ScreenCoord (Double,Double)
  deriving Show
type    CanvasCoord = (Double,Double)

radian2degree :: Double -> Double
radian2degree = (* (180 / pi))

degree2radian :: Double -> Double
degree2radian = (/ (180 / pi))

-- From http://stackoverflow.com/questions/24234609/standard-way-to-normalize-an-angle-to-%CF%80-radians-in-java
-- Normalize a radian
radian :: Double -> Double
radian t = t - pi * 2 * fromIntegral (floor((t + pi) / (pi * 2)))

-- We define that the Spherical is a sphere 5 units (meters) radius
geographicToSpherical :: Geographic -> Spherical
geographicToSpherical (Geographic (long,lat)) = Spherical (5, degree2radian lat, degree2radian long)

cartesian2Spherical :: Cartesian -> Spherical
cartesian2Spherical (Cartesian (0,0,0)) = Spherical (0,0,0)  -- choice
cartesian2Spherical (Cartesian (x,y,z)) = mkSpherical (r,t,u)
  where
      r = sqrt (x^2 + y^2 + z^2)
      t = asin (z / r)            -- polar angle, "latitude", 0 .. pi/2
      u = atan2 y x               -- azimuth angle, "longiture", -pi .. pi

mkSpherical :: (Double,Double,Double) -> Spherical 
mkSpherical (r,t,u) = mkSpherical' (r,radian t, radian u)
 where
   mkSpherical' (r,t,u) 
    | traceShow ("mkSpherical",(r,t,u)) False = undefined
--    | t < -pi / 2 = mkSpherical (r,-t,u' + pi)
--    | t >  pi / 2 = mkSpherical (r,-t,u' - pi)
    | otherwise    = Spherical (r,t,u)  -- t is +/- pi/2 (+/-90), u is +/- pi (+/-180)

spherical2Cartesian :: Spherical -> Cartesian
spherical2Cartesian (Spherical (r,t,u)) = Cartesian (x,y,z)
  where
      x = r * cos t * cos u
      y = r * cos t * sin u
      z = r * sin t

class Coordinate c where
  toSpherical :: c -> Spherical
  toCartesian :: c -> Cartesian

instance Coordinate Cartesian where
  toSpherical = cartesian2Spherical
  toCartesian = id

instance Coordinate Spherical where
  toSpherical = id
  toCartesian = spherical2Cartesian

instance Coordinate Geographic where
  toSpherical = toSpherical . geographicToSpherical
  toCartesian = toCartesian . geographicToSpherical

class Lerp a where
  lerp2 :: a -> a -> Double -> a

instance Lerp Double where
  lerp2 a b s = b * s + a * (1 - s)

instance Lerp Geographic where
  lerp2 (Geographic a) (Geographic b) s = Geographic (lerp2 a b s)

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
distance :: CanvasCoord -> CanvasCoord -> Double
distance (x,y) (x',y') = sqrt (xd * xd + yd * yd)
   where
     xd = x - x'
     yd = y - y'

------------------------------------------------------------------------
-- QC
------------------------------------------------------------------------

instance Arbitrary Cartesian where
  arbitrary = fmap Cartesian arbitrary
--  shrink (Cartesian a) = [ Cartesian c |  c <- shrink a ]

prop_c2s_then_s2c (c@(Cartesian (x,y,z))) = 
    abs (x*x + y*y + z*z) > 1e-10 ==> -- make sure you are not *too* close to the origin
--    label (show (x,y,z)) $ 
    c `eqish` spherical2Cartesian (cartesian2Spherical c)

class Eqish a where
   eqish :: a -> a -> Bool
   
instance Eqish Double where
  eqish a b = abs (a - b) < 1e-6

instance (Eqish a, Eqish b, Eqish c) => Eqish (a,b,c) where
  eqish (a,b,c) (a',b',c') = eqish a a' && eqish b b' && eqish c c'

instance Eqish Cartesian where
  eqish (Cartesian c1) (Cartesian c2) = c1 `eqish` c2

