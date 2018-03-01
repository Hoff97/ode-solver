{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Ode.Ode where

import Util.Util

class OdeSolver b where
  solveStep :: (Num a, Fractional a) => b -> (a -> a) -> a -> Double -> a

newtype Euler = Euler ()

instance OdeSolver Euler where
  solveStep (Euler ()) f a dt = a + fromRational (toRational dt)*(f a)

newtype Heun = Heun ()

instance OdeSolver Heun where
  solveStep (Heun ()) f a dt = a + fromRational (toRational dt)
    *(f a + f (a+fromRational (toRational dt)*(f a)))/2

includeTime :: Fst c Double a => (a -> Double -> a) -> c -> c
includeTime f c = fromTuple (1,f (rest c) (first c))

euler = solveStep (Euler ())
heun = solveStep (Heun ())

evolve :: (Num a, Fractional a) => (a -> a) -> Double -> Int -> a -> [a]
evolve f dt 0 a = []
evolve f dt x a = step:evolve f dt (x-1) step
  where
    step = solveStep (Euler ()) f a dt