{-# LANGUAGE ExplicitForAll         #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE Rank2Types             #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE UndecidableInstances   #-}

module HOP
  ( HOP.show
  , lam
  , app
  ) where

import           Expression

-- Extending Fegaras and Sheard's catamorphism
-- using higher order polymorphism
newtype Value =
  Fn (Value -> Value)

unFn (Fn x) = x

lam :: (Exp a -> Exp a) -> Exp a -- lambda expression
lam x = roll (Lam x)

app :: Exp a -> Exp a -> Exp a -- function applications
app x y = roll (App x y)

-- count the number of occurrences of bound variables in a sound expression
-- i.e., no Place and do not contain any non-parametric function spaces
countvarAux :: ExpF Int -> Int
countvarAux (App x y) = x + y
countvarAux (Lam f)   = f 1

countvar :: Exp Int -> Int
countvar = cata countvarAux

-- make the type parameter of Exp abstract
-- rule out badcase and badcata as in fsCata
iter0 :: (ExpF b -> b) -> (forall a. Exp a) -> b
iter0 = cata

openiter1 :: (ExpF b -> b) -> (Exp b -> Exp b) -> (b -> b)
openiter1 f x y = cata f (x (place y))

-- rule out all unsound expressions
iter1 :: (ExpF b -> b) -> (forall a. Exp a -> Exp a) -> (b -> b)
iter1 = openiter1

freevarused :: (forall a. Exp a -> Exp a) -> Bool
freevarused e =
  iter1
    (\case
       App x y -> x || y
       Lam f -> f False)
    e
    True

class Iterable a m n | m -> a, m -> n where
  openiter :: (ExpF a -> a) -> m -> n
  uniter :: (ExpF a -> a) -> n -> m

instance Iterable a (Exp a) a where
  openiter = cata
  uniter f = place

instance (Iterable a m1 n1, Iterable a m2 n2) =>
         Iterable a (m1 -> m2) (n1 -> n2) where
  openiter f x = openiter f . x . uniter f
  uniter f x = uniter f . x . openiter f

vars :: [String]
vars =
  [[i] | i <- ['a' .. 'z']] <>
  [i : Prelude.show j | j <- [1 ..], i <- ['a' .. 'z']]

showAux :: ExpF ([String] -> String) -> ([String] -> String)
showAux (App x y) vars   = "(" <> x vars <> " " <> y vars <> ")"
showAux (Lam z) (v:vars) = "(fn " <> v <> ". " <> z (const v) vars <> ")"

-- convert expressions to strings
show :: (forall a. Exp a) -> String
show e = iter0 showAux e vars
