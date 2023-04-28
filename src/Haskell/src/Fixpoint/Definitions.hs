{-# LANGUAGE RankNTypes, DeriveFunctor #-}

module Fixpoint.Definitions (
  Fix(..),
  CFix,
  Compose(..),
  Alg,
  CoAlg,
  CoAlgM,
  NatTrans,
  natTransAlg,
  natTransCoAlg,
  NFixF(..),
  NFix,
  toFix,
  maxDepth
) where

import Data.Fix (Fix(..))
import Data.Functor.Foldable (cata)
import Data.Functor.Compose (Compose(..))

type Alg f a = f a -> a
type CoAlg f a = a -> f a
type CoAlgM f m a = a -> f (m a)

-- Natural transformations
type NatTrans f g = forall r. f r -> g r

natTransAlg :: NatTrans f g -> Alg f (Fix g)
natTransAlg nt f = Fix $ nt f

natTransCoAlg :: NatTrans f g -> CoAlg g (Fix f)
natTransCoAlg nt f = nt $ unFix f

-- Composition of two functors
type CFix w f = Fix (Compose w f)

-- NFix. Functor f composed n times with itself
data NFixF f x r = N1 (f x)
                 | NS (f r)
  deriving (Eq, Show, Functor)

type NFix f x = Fix (NFixF f x)

toFix :: (Functor f) => NFix f (Fix f) -> Fix f
toFix = cata go
  where
  go :: Alg (NFixF f (Fix f)) (Fix f)
  go (N1 x) = Fix x
  go (NS x) = Fix x

maxDepth :: forall f r. (Functor f, Foldable f) => NFix f r -> Int
maxDepth = cata go
  where go :: Foldable f => Alg (NFixF f x) Int
        go (N1 _) = 1
        go (NS x) = 1 + foldr max 0 x

-- -- Patterns
-- data NFixPatF f r = PAny
--                   | PNode (f () -> Bool) (f r)

-- type NFixPat f x = 