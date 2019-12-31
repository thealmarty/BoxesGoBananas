module ExpressionsEg where

import           Expression
import           HOP

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

-- \x.\y. x y
appXY :: Exp a
appXY = lam (\x -> lam (\y -> app x y))
