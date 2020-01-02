{-# LANGUAGE Rank2Types #-}

module ParallelReduction
  ( parallel
  ) where

import           Expression
import           HOP

-- pairing: as we iterate over the terms 2 results are produced and stored here
data PAR a =
  PAR
    { par   :: Exp a -- result of the reduction
    , apply :: Exp a -> Exp a -- a function that we build up for the application case
    }

parAux :: ExpF (PAR a) -> PAR a
parAux (Lam f) = PAR {par = lam (par . f . var), apply = par . f . var}
  where
    var :: Exp a -> PAR a
    var x = PAR {par = x, apply = app x}
parAux (App x y) = PAR {par = apply x (par y), apply = app (apply x (par y))}

parallel :: (forall v. Exp v) -> (forall v. Exp v)
parallel x = par (iter0 parAux x)
