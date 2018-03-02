{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
module Examples.PendulumState where

import Examples.DoublePendulum
import Data.Array as Arr
import Ode.Ode
import Data.ByteString
import Graphics.Image as Img
import Data.Fixed
import Util.Util

--width (rho), resolution (rho), dt, t
simulateMany :: Double -> Double -> Double -> Double -> Arr.Array (Int,Int) Model
simulateMany r dr dt t = fmap simTime arr
  where
    n = floor (r/dr)
    arr = array ((-n,-n),(n,n)) [((i,j),start i j) | i <- [-n..n], j <- [-n..n]]
    start i j = (0.0,(fromIntegral i*dr,fromIntegral j*dr),(0,0))
    simTime !s = Ode.Ode.simulate (RungeKutta ()) f dt t s
    f = dpt 0.5 9.81

arrToPicture :: Arr.Array (Int,Int) Model -> Img.Image VU RGB Double
arrToPicture !arr = makeImageR VU (b-a,d-c) (\(i, j) -> modelToColor (arr!(i+a,j+c)))
  where
    modelToColor (_,(r1,r2),_) = PixelRGB r g b
      where
        (r,g,b) = ((r1+2*pi)/(4*pi),(r2+2*pi)/(4*pi),0)
    ((a,c),(b,d)) = bounds arr

test = writeImage "./test4.png" picture

picture = arrToPicture arr
  where
    arr = simulateMany (1.5*pi) 0.05 0.1 3
