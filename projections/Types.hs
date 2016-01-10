{-# LANGUAGE ScopedTypeVariables #-}
module Types where

import Test.QuickCheck

type Longitude = Double -- -180 .. 180
type Latitude  = Double --  -90 .. 90
newtype Geographic = Geographic (Longitude,Latitude)

type ScreenX = Double         -- -1 .. 1, + on right
type ScreenY = Double         -- -1 .. 1, + on top

type Distance    = Double     -- nominally meters
type Inclination = Double     -- In Radians, 0 is ahead, + is up
type Azimuth     = Double     -- In Radians, 0 is ahead, + is right

newtype Spherical = Spherical (Distance,Inclination,Azimuth)
newtype Cartesian = Cartesian (Double,Double,Double)         -- in nominal meters

radian2degree :: Double -> Double
radian2degree = (* (180 / pi))

degree2radian :: Double -> Double
degree2radian = (/ (180 / pi))

-- We define that the Spherical is a sphere 5 units (meters) radius
geographicToSpherical :: Geographic -> Spherical
geographicToSpherical (Geographic (long,lat)) = Spherical (degree2radian long, degree2radian lat, 5)

cartesian2Spherical :: Cartesian -> Spherical
cartesian2Spherical (Cartesian (0,0,0)) = Spherical (0,0,0)  -- choice
cartesian2Spherical (Cartesian (x,y,z)) = Spherical (r,t,u)
  where
      r = sqrt (x^2 + y^2 + z^2)
      t = acos (z / r)
      u = atan2 y x

spherical2Cartesian :: Spherical -> Cartesian
spherical2Cartesian (Spherical (r,t,u)) = Cartesian (x,y,z)
  where
      x = r * sin t * cos u
      y = r * sin t * sin u
      z = r * cos t

------------------------------------------------------------------------
-- QC
------------------------------------------------------------------------

prop_c2s_then_s2c (c@(Cartesian (x,y,z))) = 
    abs (x*x + y*y + z*z) > 1e-10 ==> -- make sure you are not *too* close to the origin
    label (show (x,y,z)) $ 
    c `eqish` spherical2Cartesian (cartesian2Spherical c)

class Eqish a where
   eqish :: a -> a -> Bool
   
instance Eqish Double where
  eqish a b = abs (a - b) < 1e-10

instance (Eqish a, Eqish b, Eqish c) => Eqish (a,b,c) where
  eqish (a,b,c) (a',b',c') = eqish a a' && eqish b b' && eqish c c'

instance Eqish Cartesian where
  eqish (Cartesian c1) (Cartesian c2) = c1 `eqish` c2

