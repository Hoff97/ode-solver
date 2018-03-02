{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Util.Util where

import Data.Fixed

class Fst a b c | a -> b c where
  first :: a -> b
  rest :: a -> c
  fromTuple :: (b,c) -> a

instance Fst (a,b) a b where
  first (a,b) = a
  rest (a,b) = b
  fromTuple = id

instance Fst (a,b,c) a (b,c) where
  first (a,b,c) = a
  rest (a,b,c) = (b,c)
  fromTuple (a,(b,c)) = (a,b,c)

instance Fst (a,b,c,d) a (b,c,d) where
  first (a,b,c,d) = a
  rest (a,b,c,d) = (b,c,d)
  fromTuple (a,(b,c,d)) = (a,b,c,d)

class Second a d where
  second :: a -> d

instance (Fst a b c, Fst c d e) => Second a d where
  second a = first (rest a)

class Third a d where
  third :: a -> d

instance (Fst a b c, Fst c d e, Fst e f g) => Third a f where
  third a = first (rest (rest a))

instance {-# OVERLAPPABLE #-} (Fst a b c, Num b, Num c) => Num a where
  a + b = fromTuple (first a + first b, rest a + rest b)
  a * b = fromTuple (first a * first b, rest a * rest b)
  abs a = fromTuple (abs (first a), abs (rest a))
  signum a = fromTuple (signum (first a), signum (rest a))
  fromInteger a = fromTuple (fromInteger a, fromInteger a)
  negate a = fromTuple (negate (first a), negate (rest a))

instance {-# OVERLAPPABLE #-} (Fst a b c, Fractional b, Fractional c) => Fractional a where
  fromRational a = fromTuple (fromRational a, fromRational a)
  a / b = fromTuple (first a / first b, rest a / rest b)

instance {-# OVERLAPPABLE #-} (Fst a b c, Floating b, Floating c) => Floating a where
  pi = fromTuple (pi, pi)
  exp a = fromTuple (exp (first a), exp (rest a))
  log a = fromTuple (log (first a), log (rest a))
  sin a = fromTuple (sin (first a), sin (rest a))
  cos a = fromTuple (cos (first a), cos (rest a))
  asin a = fromTuple (asin (first a), asin (rest a))
  acos a = fromTuple (acos (first a), acos (rest a))
  atan a = fromTuple (atan (first a), atan (rest a))
  sinh a = fromTuple (sinh (first a), sinh (rest a))
  cosh a = fromTuple (cosh (first a), cosh (rest a))
  acosh a = fromTuple (acosh (first a), acosh (rest a))
  asinh a = fromTuple (asinh (first a), asinh (rest a))
  atanh a = fromTuple (atanh (first a), atanh (rest a))

type D3 = (Double,Double,Double)

hsvToRGB :: D3 -> D3
hsvToRGB (h,s,v) = case hi of
  1 -> (q,v,p)
  2 -> (p,v,t)
  3 -> (p,q,v)
  4 -> (t,p,v)
  5 -> (v,p,q)
  _ -> (v,t,p)
  where
    h' = (h - fromIntegral (floor $ h/360)*360)
    hi = floor $ h'/60
    p = v*(1-s)
    q = v*(1-s*f)
    t = v*(1-s*(1-f))
    f = (h'/60-fromIntegral hi)/60
