{-# LANGUAGE LambdaCase #-}

module Expression
  ( Rec
  , ExpF(..)
  , Exp
  , roll
  , place
  , cata
  ) where

data Rec a b
  = Roll (a (Rec a b))
  | Place b

data ExpF a -- terms are either lam expressions or function applications
  = Lam (a -> a)
  | App a a

type Exp a = Rec ExpF a

xmapExpF :: (a -> b, b -> a) -> (ExpF a -> ExpF b, ExpF b -> ExpF a)
xmapExpF (f, g) =
  ( \case
      Lam x -> Lam (f . x . g)
      App y z -> App (f y) (f z)
  , \case
      Lam x -> Lam (g . x . f)
      App y z -> App (g y) (g z))

cata :: (ExpF a -> a) -> Exp a -> a -- catamorphism
cata f (Roll x)  = f (fst (xmapExpF (cata f, Place)) x)
cata f (Place x) = x

roll :: ExpF (Exp a) -> Exp a
roll = Roll

place :: a -> Exp a
place = Place
