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

-- count the number of occurrences of bound variables in a sound expression
-- i.e., no Place and do not contain any non-parametric function spaces
countvarAux :: ExpF Int -> Int
countvarAux (App x y) = x + y
countvarAux (Lam f)   = f 1

countvar :: Exp Int -> Int
countvar = cata countvarAux

-- e.g. of an expression that countvar does not work on
badplace :: Exp Int
badplace = lam (\x -> Place 2)

-- unsound/not parametric functions
badcata :: Exp Int
badcata =
  lam
    (\x ->
       if countvar x == 1
         then app x x
         else x)

badcase :: Exp a
badcase =
  lam
    (\x ->
       case x of
         Roll (App v w) -> app x x
         Roll (Lam f)   -> x
         Place v        -> x)

-- Example expressions
identity :: Exp a
identity = lam id

-- \x.x (\x.x)
idAppid :: Exp a
idAppid = app identity identity
