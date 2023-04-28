{-# LANGUAGE TypeOperators #-}

module Abstract.Dissection (
  Clown,
  Joker,
  Diss
) where

import Data.Kind
import Data.Bifunctor (Bifunctor(..))
import Abstract.Polyfunctors
import Abstract.PolyBifunctors

-- Clowns ... a bifunctor that acts as a functor, with everything on the first parameter
newtype Clown f a b = Clown (f a)
instance (Functor f) => Bifunctor (Clown f) where
    bimap f _ (Clown x) = Clown (fmap f x)

-- Jokers ... a bifunctor that acts as a functor, with everything on the second parameter
newtype Joker f a b = Joker (f b)
instance (Functor f) => Bifunctor (Joker f) where
    bimap _ g (Joker x) = Joker (fmap g x)

type family Diss p :: Type -> Type -> Type

type instance Diss (K c a) = Zero2
type instance Diss (I a) = Unit2
type instance Diss ((x1 :+: x2) a) = Diss (x1 a) :++: Diss (x2 a)
type instance Diss ((x1 :*: x2) a) = (Diss (x1 a) :**: Joker x2) :++: (Clown x1 :**: Diss (x2 a))
type instance Diss ((x1 :.: x2) a) = Diss (x2 a) :**: Comp2 (Diss (x1 a)) (Clown x2) (Joker x2)

