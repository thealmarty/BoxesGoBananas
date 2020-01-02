module EgExpressions where

import           Expression
import           HOP
import           ParallelReduction

-- Example expressions
-- \x.x
identity :: Exp a
identity = lam id

-- \x.\y.x
k :: Exp a
k = lam (\x -> lam (\y -> x))

-- \x.x (\x.x)
idAppid :: Exp a
idAppid = app identity identity

-- \x.\y.x y
appXY :: Exp a
appXY = lam (\x -> lam (\y -> app x y))

-- \x.x x
appXToX :: Exp a
appXToX = lam (\x -> app x x)

-- (\x.x x) \x.x
appXToXid = app appXToX identity
