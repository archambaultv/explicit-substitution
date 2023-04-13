{-# LANGUAGE DeriveFunctor #-}

module ELambdaCalculus
    ( LTermF(..),
      LTerm,
      ETermF(..),
      ETerm,
      rename,
      pushSub,
      beta
    ) where

import Data.Fix (Fix(..))
import FLambdaCalculus (LTermF(..), LTerm, Rename, countBinders)

data ETermF r = Plain (LTermF r)
              | ESubsF r (Int -> r)
  deriving (Functor)

type ETerm = Fix ETermF

-- Build an EVar
evar :: Int -> ETerm
evar = Fix . Plain . LVarF

rename :: Rename -> ETerm -> ETerm
rename r x = Fix $ ESubsF x (evar . r)

-- Push the substitution one layer down
pushSub :: ETerm -> ETerm
pushSub (Fix (ESubsF (Fix (Plain (LVarF i))) s)) = s i
pushSub (Fix ((ESubsF (Fix (Plain x)) s))) =
  let myBinder = countBinders x
      s' = if myBinder == 0
           then s
           else \i -> if i < myBinder then evar i else rename (+ myBinder) $ s (i - myBinder)
  in Fix $ Plain $ fmap (\r -> Fix (ESubsF r s')) x
pushSub (Fix (ESubsF (Fix (ESubsF x s2)) s)) =
  let x' = pushSub (Fix (ESubsF x s2))
  in pushSub (Fix (ESubsF x' s))
pushSub x = x

-- Perform beta application
beta :: ETerm -> ETerm -> ETerm
beta body arg = Fix $ ESubsF body (\i -> if i == 0 then arg else evar (i - 1))