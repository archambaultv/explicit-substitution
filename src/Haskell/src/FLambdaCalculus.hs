{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, TemplateHaskell, TypeFamilies #-}

module FLambdaCalculus
    ( LTermF(..),
      LTerm,
      Rename,
      Substitution,
      countBinders,
      rename,
      substitute,
      beta
    ) where

import Text.Show.Deriving (deriveShow1)
import Data.Fix (Fix(..))
import Data.Functor.Compose (Compose(..))
import Data.Functor.Foldable (cata)

import Fix

data LTermF r = LVarF Int
              | LAbsF r
              | LAppF r r
  deriving (Show, Eq, Functor, Foldable, Traversable)

$(deriveShow1 ''LTermF)

type LTerm = Fix LTermF
type Rename = Int -> Int
type Substitution = Int -> LTerm

countBinders :: LTermF r -> Int
countBinders (LAbsF _) = 1
countBinders _ = 0

withBinders :: LTerm -> CFix (Ann Int) LTermF
withBinders t = withBinders' 0 t

withBinders' :: Int -> LTerm -> CFix (Ann Int) LTermF
withBinders' n t = cata go t n
  where go :: Alg LTermF (Int -> CFix (Ann Int) LTermF)
        go x = \m -> 
                let myBinder = countBinders x
                in Fix $ Compose (m, fmap (\f -> f (m + myBinder)) x)

rename :: Rename -> LTerm -> LTerm
rename r t = cata go $ withBinders t
  where go :: Alg (Compose (Ann Int) LTermF) LTerm
        go (Compose (n, LVarF i)) = Fix $ if i < n then LVarF i else LVarF $ r (i - n) + n
        go (Compose (_, x)) = Fix x

substitute :: Substitution -> LTerm -> LTerm
substitute s t = cata go $ withBinders t
  where go :: Alg (Compose (Ann Int) LTermF) LTerm
        go (Compose (n, LVarF i)) = rename (+ n) $ s (i - n)
        go (Compose (_, x)) = Fix x

-- Perform beta application
beta :: LTerm -> LTerm -> LTerm
beta body arg = substitute (\i -> if i == 0 then arg else Fix $ LVarF (i - 1)) body