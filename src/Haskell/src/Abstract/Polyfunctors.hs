{-# LANGUAGE TypeOperators, FlexibleInstances, FlexibleContexts, 
    UndecidableInstances, MultiParamTypeClasses, ScopedTypeVariables, InstanceSigs #-}

-- Poly functors can be converted to AST
-- This is a proof of concept. The limited levels of Fixpoint.NFix
-- obviously limit the functionnality of the code below

module Abstract.Polyfunctors (
  I(..),
  K(..),
  (:+:)(..),
  (:*:)(..),
  (:.:)(..),
  PolyLabel(..)
) where

import Fixpoint
import Abstract.AST

-- Identity functor
newtype I a = I a
instance Functor I where
    fmap f (I x) = I (f x)

-- Constant functor
newtype K c a = K c
instance Functor (K c) where
    fmap _ (K c) = K c

-- Sum functor
infixl 6 :+:
data (f :+: g) a = InL (f a) | InR (g a)
instance (Functor f, Functor g) => Functor (f :+: g) where
    fmap f (InL fx) = InL (fmap f fx)
    fmap f (InR gy) = InR (fmap f gy)

-- Product functor
infixl 7 :*:
data (f :*: g) a = f a :*: g a
instance (Functor f, Functor g) => Functor (f :*: g) where
    fmap f (fx :*: gy) = fmap f fx :*: fmap f gy

-- Composition functor
infixr 9 :.:
newtype (f :.: g) a = Cmp (f (g a))
instance (Functor f, Functor g) => Functor (f :.: g) where
    fmap f (Cmp fgx) = Cmp (fmap (fmap f) fgx)

data PolyLabel = Id -- Identity
               | L -- Left
               | R -- Right
               | P -- Product
              -- | C -- Compose

instance ToAst (K c) PolyLabel c where
  toAst (K c) = N1 $ LeafF c

instance ToAst I PolyLabel c where
  toAst (I x) = N1 $ NodeF Id [x]

instance (ToAst f PolyLabel c, ToAst g PolyLabel c) => ToAst (f :+: g) PolyLabel c where
  toAst (InL x) = 
    let l' = toAst x
    in case l' of
        N1 f' -> N2 $ NodeF L [f']
        N2 _ -> error "Limited to two levels"
  toAst (InR x) =
    let r' = toAst x
    in case r' of
        N1 g' -> N2 $ NodeF R [g']
        _ -> error "Limited to two levels"

instance (ToAst f PolyLabel c, ToAst g PolyLabel c) => ToAst (f :*: g) PolyLabel c where
  toAst (x1 :*: x2) = 
    let x1' = toAst x1
        x2' = toAst x2
    in case (x1', x2') of
        (N1 f', N1 g') -> N2 $ NodeF P [f', g']
        _ -> error "Limited to two levels"

instance (Functor f, ToAst f PolyLabel c, ToAst g PolyLabel c) => ToAst (f :.: g) PolyLabel c where
  toAst :: forall r. (f :.: g) r ->  NFix (AstF PolyLabel c) r
  toAst (Cmp x) = 
    let fg' :: f (NFix (AstF PolyLabel c) r)
        fg' = fmap toAst x
        f'g' :: NFix (AstF PolyLabel c) (NFix (AstF PolyLabel c) r)
        f'g' = toAst fg'
    in case f'g' of
        N1 (LeafF y) -> N1 (LeafF y)
        N1 (NodeF n xs) ->
          let xs' = map fromN1 xs
          in N2 $ NodeF n xs'
        _ -> error "Limited to two levels"

    where fromN1 :: NFix (AstF PolyLabel c) r -> AstF PolyLabel c r
          fromN1 (N1 y) = y
          fromN1 _ = error "Limited to two levels"