{-# LANGUAGE LambdaCase #-}

-- Fegaras and Sheard's catamorphism for parametric functions
-- Similar to MH's except that instead of using anamorphism,
-- a place holder is used to store the expanded data structures.
newtype Value =
  Fn (Value -> Value)

unFn (Fn x) = x

data Rec a b
  = Roll (a (Rec a b))
  | Place b -- place holder for storing original argument

data ExpF a -- terms are either lam expressions or function applications
  = Lam (a -> a)
  | App a a

type Exp a = Rec ExpF a

lam :: (Exp a -> Exp a) -> Exp a -- lambda expression
lam x = Roll (Lam x)

app :: Exp a -> Exp a -> Exp a -- function applications
app x y = Roll (App x y)

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
