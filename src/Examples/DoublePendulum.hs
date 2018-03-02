{-# LANGUAGE FlexibleContexts #-}
module Examples.DoublePendulum where

import Util.Util
import Ode.Ode
import Graphics.Gloss

type F = Float
type F2 = (F,F)
type F3 = (F,F,F)
type D = Double
type D2 = (D,D)
type D3 = (D,D,D)

dToF :: Double -> Float
dToF = fromRational . toRational

doublePendulum :: D -> D -> (D2,D2) -> (D2,D2)
doublePendulum l m ((r1,r2),(p1,p2)) = ((rho1',rho2'),(p1',p2'))
  where
    rho1' = 6/(m*l*l)*(2*p1-3*cos (r1-r2)*p2)/(16-9*(cos (r1-r2)**2))
    rho2' = 6/(m*l*l)*(8*p2-3*cos (r1-r2)*p1)/(16-9*(cos (r1-r2)**2))
    p1' = (-m*l*l)/2*(rho1'*rho2'*sin (r1-r2)+3*g/l*sin r1)
    p2' = (-m*l*l)/2*(-rho1'*rho2'*sin (r1-r2)+g/l*sin r2)
    g = 9.81

dpt :: D -> D -> (Double,D2,D2) -> (Double,D2,D2)
dpt l g = includeTime (\a t -> doublePendulum l g a)

--t, (rho1,rho2), (p1,p2)
type Model = (Double,D2,D2)

draw :: Model -> Picture
draw (t,(r1,r2),b) = pictures [line [(0,0), (x1,y1)],translate x1 y1 $ circleSolid 10,
                              translate x1 y1 $ pictures [line [(0,0), (x2,y2)],translate x2 y2 $ circleSolid 10]]
  where
    (r1',r2') = (dToF r1,dToF r2)
    (x1,y1) = (100*sin r1',-100*cos r1')
    (x2,y2) = (100*sin r2',-100*cos r2')

sim :: ((Double,D2,D2) -> (Double,D2,D2)) -> Float -> Model -> Model
sim f dt m = solveStep (RungeKutta ()) f m (fromRational $ toRational dt)

f = dpt 0.25 9.81

start = (0.0,(0,0),(0,0))

prog = Graphics.Gloss.simulate
        (InWindow
               "Hello World"
                (400, 400)
                (200, 200))
        white 128 start draw (\p -> sim f)
