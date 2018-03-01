{-# LANGUAGE FlexibleContexts #-}
module Examples.DoublePendulum where

import Util.Util
import Ode.Ode
import Graphics.Gloss

type F = Float
type F2 = (F,F)
type F3 = (F,F,F)
type D = Double

doublePendulum :: F -> F -> (F2,F2) -> (F2,F2)
doublePendulum l m ((r1,r2),(p1,p2)) = ((rho1',rho2'),(p1',p2'))
  where
    rho1' = 6/(m*l*l)*(2*p1-3*cos (r1-r2)*p2)/(16-9*(cos (r1-r2)**2))
    rho2' = 6/(m*l*l)*(8*p2-3*cos (r1-r2)*p1)/(16-9*(cos (r1-r2)**2))
    p1' = (-m*l*l)/2*(rho1'*rho2'*sin (r1-r2)+3*g/l*sin r1)
    p2' = (-m*l*l)/2*(-rho1'*rho2'*sin (r1-r2)+g/l*sin r2)
    g = 9.81

dpt :: Float -> Float -> (Double,F2,F2) -> (Double,F2,F2)
dpt l g = includeTime (\a t -> doublePendulum l g a)

--t, (rho1,rho2), (p1,p2)
type Model = (Double,F2,F2)

draw :: Model -> Picture
draw (t,(r1,r2),b) = pictures [line [(0,0), (x1,y1)],translate x1 y1 $ circleSolid 10,
                              translate x1 y1 $ pictures [line [(0,0), (x2,y2)],translate x2 y2 $ circleSolid 10]]
  where
    (x1,y1) = (100*sin r1,-100*cos r1)
    (x2,y2) = (100*sin r2,-100*cos r2)

sim :: ((Double,F2,F2) -> (Double,F2,F2)) -> Float -> Model -> Model
sim f dt m = solveStep (Heun ()) f m (fromRational $ toRational dt)

f = dpt 0.25 9.81

init = ((0,0),(0,0))

prog = simulate
        (InWindow
               "Hello World"
                (400, 400)
                (200, 200))
        white 128 (0.0,90/180*pi,0) draw (\p -> sim f)
