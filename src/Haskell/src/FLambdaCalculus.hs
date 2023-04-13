{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, TemplateHaskell, PatternSynonyms #-}

module FLambdaCalculus
    ( FTermF(..),
      FTerm,
      pattern FVar,
      pattern FAbs,
      pattern FApp,
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

data FTermF r = FVarF Int
              | FAbsF r
              | FAppF r r
  deriving (Show, Eq, Functor, Foldable, Traversable)

$(deriveShow1 ''FTermF)

type FTerm = Fix FTermF
type Rename = Int -> Int
type Substitution = Int -> FTerm

pattern FVar :: Int -> FTerm
pattern FVar x = Fix (FVarF x)

pattern FAbs :: FTerm -> FTerm
pattern FAbs x = Fix (FAbsF x)

pattern FApp :: FTerm -> FTerm -> FTerm
pattern FApp e1 e2 = Fix (FAppF e1 e2)

countBinders :: FTermF r -> Int
countBinders (FAbsF _) = 1
countBinders _ = 0

withBinders :: FTerm -> CFix (Ann Int) FTermF
withBinders t = withBinders' 0 t

withBinders' :: Int -> FTerm -> CFix (Ann Int) FTermF
withBinders' n t = cata go t n
  where go :: Alg FTermF (Int -> CFix (Ann Int) FTermF)
        go x = \m -> 
                let myBinder = countBinders x
                in Fix $ Compose (m, fmap (\f -> f (m + myBinder)) x)

rename :: Rename -> FTerm -> FTerm
rename r t = cata go $ withBinders t
  where go :: Alg (Compose (Ann Int) FTermF) FTerm
        go (Compose (n, FVarF i)) = if i < n then FVar i else FVar $ r (i - n) + n
        go (Compose (_, x)) = Fix x

substitute :: Substitution -> FTerm -> FTerm
substitute s t = cata go $ withBinders t
  where go :: Alg (Compose (Ann Int) FTermF) FTerm
        go (Compose (n, FVarF i)) = rename (+ n) $ s (i - n)
        go (Compose (_, x)) = Fix x

-- Perform beta application
beta :: FTerm -> FTerm -> FTerm
beta body arg = substitute (\i -> if i == 0 then arg else FVar (i - 1)) body