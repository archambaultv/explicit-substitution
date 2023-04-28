{-# LANGUAGE TypeOperators #-}

module Abstract.PolyBifunctors (
  K2(..),
  Zero2,
  Unit2,
  Fst(..),
  Snd(..),
  (:++:)(..),
  (:**:)(..),
  Comp2(..)
) where

import Data.Bifunctor (Bifunctor(..))
import Data.Void (Void)

-- Constant bifunctor
newtype K2 c a b = K2 c
instance Bifunctor (K2 c) where
    bimap _ _ (K2 c) = K2 c

type Zero2 = K2 Void
type Unit2 = K2 ()

-- First bifunctor
newtype Fst a b = Fst a
instance Bifunctor Fst where
    bimap f _ (Fst x) = Fst (f x)

-- Second bifunctor
newtype Snd a b = Snd b
instance Bifunctor Snd where
    bimap _ g (Snd x) = Snd (g x)

-- Sum bifunctor
infixl 6 :++:
data (f :++: g) a b = InL (f a b) | InR (g a b)
instance (Bifunctor f, Bifunctor g) => Bifunctor (f :++: g) where
    bimap f g (InL x) = InL (bimap f g x)
    bimap f g (InR x) = InR (bimap f g x)

-- Product bifunctor
infixl 7 :**:
data (f :**: g) a b = f a b :**: g a b
instance (Bifunctor f, Bifunctor g) => Bifunctor (f :**: g) where
    bimap f g (x1 :**: x2) = bimap f g x1 :**: bimap f g x2

-- Composition bifunctor
newtype Comp2 f g1 g2 a b = Comp2 (f (g1 a b) (g2 a b))
instance (Bifunctor f, Bifunctor g1, Bifunctor g2) => Bifunctor (Comp2 f g1 g2) where
    bimap f g (Comp2 x) = Comp2 (bimap (bimap f g) (bimap f g) x)