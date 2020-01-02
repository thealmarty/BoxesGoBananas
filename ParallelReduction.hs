{-# LANGUAGE Rank2Types #-}

module ParallelReduction
  ( parallel
  , cps
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

-- cps-conversion
data CPS a =
  CPS -- the following are mutually recursive closure conversions
    { cpsmeta :: (Exp a -> Exp a) -> Exp a -- for a term of type Exp a -> Exp a
    , cpsobj  :: Exp a -> Exp a -- for a term of type Exp a
    }

-- value describes a value's CPS conversion
value :: Exp a -> CPS a
value x = CPS {cpsmeta = \k -> k x, cpsobj = \c -> app c x}

cpsAux :: ExpF (CPS a) -> CPS a
cpsAux (App e1 e2) = CPS {cpsmeta = appexp . lam, cpsobj = appexp}
  where
    appexp c = cpsmeta e1 (\y1 -> cpsmeta e2 (\y2 -> app (app y1 y2) c))
cpsAux (Lam f) = value (lam (lam . cpsobj . f . value))

cps :: (forall a. Exp a) -> (forall a. Exp a)
cps x = lam (\a -> cpsmeta (iter0 cpsAux x) (\m -> app a m))
