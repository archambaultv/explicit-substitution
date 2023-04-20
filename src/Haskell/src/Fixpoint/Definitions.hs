{-# LANGUAGE RankNTypes #-}

module Fixpoint.Definitions (
  Fix(..),
  Alg,
  CoAlg,
  CoAlgM,
  NatTrans,
  NatTransK,
  natTransAlg,
  natTransCoAlg,
) where

import Data.Fix (Fix(..))

type Alg f a = f a -> a
type CoAlg f a = a -> f a
type CoAlgM f m a = a -> f (m a)

-- Natural transformations
type NatTrans f g = forall r. f r -> g r
type NatTransK f a = forall r. f r -> a -- Isomorphic to the Const functor, but easier to work with

natTransAlg :: NatTrans f g -> Alg f (Fix g)
natTransAlg nt f = Fix $ nt f

natTransCoAlg :: NatTrans f g -> CoAlg g (Fix f)
natTransCoAlg nt f = nt $ unFix f