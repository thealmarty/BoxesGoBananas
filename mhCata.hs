{-# LANGUAGE LambdaCase #-}

-- Meijer/Hutton catamorphism/anamorphism for datatypes; adapted from Boxes go bananas
newtype Value =
  Fn (Value -> Value)

unFn (Fn x) = x

newtype Rec a =
  Roll (a (Rec a)) -- recursion

data ExpF a -- terms are either lam expressions or function applications
  = Lam (a -> a)
  | App a a

type Exp = Rec ExpF

lam :: (Exp -> Exp) -> Exp -- lambda expression
lam x = Roll (Lam x)

app :: Exp -> Exp -> Exp -- function applications
app x y = Roll (App x y)

xmapExpF :: (a -> b, b -> a) -> (ExpF a -> ExpF b, ExpF b -> ExpF a)
xmapExpF (f, g) =
  ( \case
      Lam x -> Lam (f . x . g)
      App y z -> App (f y) (f z)
  , \case
      Lam x -> Lam (g . x . f)
      App y z -> App (g y) (g z))

cata :: (ExpF a -> a) -> (a -> ExpF a) -> Rec ExpF -> a -- catamorphism
cata f g (Roll x) = f (fst (xmapExpF (cata f g, ana f g)) x)

ana :: (ExpF a -> a) -> (a -> ExpF a) -> a -> Rec ExpF
ana f g x = Roll (snd (xmapExpF (cata f g, ana f g)) (g x))

evalAux :: ExpF Value -> Value
evalAux (Lam f)   = Fn f
evalAux (App x y) = unFn x y

unevalAux :: Value -> ExpF Value
unevalAux (Fn f) = Lam f

eval :: Exp -> Value
eval = cata evalAux unevalAux

-- Example expressions
identity :: Exp
identity = lam id

idAppid :: Exp
idAppid = app identity identity
