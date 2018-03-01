{-# LANGUAGE FlexibleContexts #-}
module Examples.Pendulum where

import Util.Util
import Ode.Ode
import Graphics.Gloss

type F = Float
type F2 = (F,F)
type F3 = (F,F,F)
type D = Double

singlePendulum :: F -> F -> F2 -> F2
singlePendulum l g (ro,ro') = (ro',-g/l*sin ro)

spt :: Float -> Float -> (Double,Float,Float) -> (Double,Float,Float)
spt l g = includeTime (\a t -> singlePendulum l g a)

--t, rho, rho'
type Model = (Double,Float,Float)

draw :: Model -> Picture
draw (t,rho,rho') = pictures [line [(0,0), (x,y)],translate x y $ circleSolid 10]
  where
    (x,y) = (100*sin rho,-100*cos rho)

sim :: ((Double,Float,Float) -> (Double,Float,Float)) -> Float -> Model -> Model
sim f dt m = solveStep (Heun ()) f m (fromRational $ toRational dt)

f = spt 0.25 9.81

prog = simulate
        (InWindow
               "Hello World"
                (400, 400)
                (200, 200))
        white 128 (0.0,90/180*pi,0) draw (\p -> sim f)
