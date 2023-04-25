{-# LANGUAGE RankNTypes #-}

module Fixpoint.Definitions (
  Fix(..),
  NFix(..),
  Alg,
  CoAlg,
  CoAlgM,
  NatTrans,
  natTransAlg,
  natTransCoAlg,
) where

import Data.Fix (Fix(..))

-- To go around the lact of dependent types
-- This should be a dependent type with index n
-- We don't need more than 2 level in this project
data NFix f r = N1 (f r)
              | N2 (f (f r))

type Alg f a = f a -> a
type CoAlg f a = a -> f a
type CoAlgM f m a = a -> f (m a)

-- Natural transformations
type NatTrans f g = forall r. f r -> g r

natTransAlg :: NatTrans f g -> Alg f (Fix g)
natTransAlg nt f = Fix $ nt f

natTransCoAlg :: NatTrans f g -> CoAlg g (Fix f)
natTransCoAlg nt f = nt $ unFix f