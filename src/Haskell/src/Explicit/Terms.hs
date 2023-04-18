{-# LANGUAGE DeriveFunctor, PatternSynonyms #-}

module Explicit.Terms
    ( 
      -- FTermF(..),
      -- FTerm,
      -- ETermF(..),
      -- ETerm,
      -- Rename,
      -- Substitution,
      -- pattern EVar,
      -- pattern EAbs,
      -- pattern EApp,
      -- pattern ESubs,
      -- pattern EPlain,
      -- mergeSub,
      -- rename,
      -- pushSub,
      -- pushMergeSub,
      -- beta
    ) where

-- import Data.Fix (Fix(..))
-- import DeBruijn.Terms (FTermF(..), FTerm, Rename, countBinders)

-- data ETermF r = Plain (FTermF r)
--               | ESubsF r (Int -> r)
--   deriving (Functor)

-- type ETerm = Fix ETermF
-- type Substitution = Int -> ETerm

-- pattern EVar :: Int -> ETerm
-- pattern EVar x = Fix (Plain (FVarF x))

-- pattern EAbs :: ETerm -> ETerm
-- pattern EAbs x = Fix (Plain (FAbsF x))

-- pattern EApp :: ETerm -> ETerm -> ETerm
-- pattern EApp e1 e2 = Fix (Plain (FAppF e1 e2))

-- pattern EPlain :: FTermF ETerm -> ETerm
-- pattern EPlain x = Fix (Plain x)

-- pattern ESubs :: ETerm -> Substitution -> ETerm
-- pattern ESubs e s = Fix (ESubsF e s)

-- rename :: Rename -> ETerm -> ETerm
-- rename r x = Fix $ ESubsF x (EVar . r)

-- -- Push the substitution one layer down
-- pushSub :: ETerm -> ETerm
-- pushSub (ESubs (EVar i) s) = s i
-- pushSub (ESubs (EPlain x) s) =
--   let myBinder = countBinders x
--       s' = if myBinder == 0
--            then s
--            else \i -> if i < myBinder then EVar i else rename (+ myBinder) $ s (i - myBinder)
--   in EPlain $ fmap (\r -> ESubs r s') x
-- pushSub (ESubs (ESubs x s1) s2) =
--   let x' = pushSub (ESubs x s1)
--   in pushSub (ESubs x' s2)
-- pushSub x = x

-- mergeSub :: Substitution -> Substitution -> Substitution
-- mergeSub s1 s2 n = 
--   case s1 n of
--     (EVar i) -> s2 i
--     x -> ESubs x s2

-- pushMergeSub :: ETerm -> ETerm
-- pushMergeSub (ESubs (EVar i) s) = s i
-- pushMergeSub (ESubs (EPlain x) s) =
--   let myBinder = countBinders x
--       s' = if myBinder == 0
--            then s
--            else \i -> if i < myBinder then EVar i else rename (+ myBinder) $ s (i - myBinder)
--   in EPlain $ fmap (\r -> ESubs r s') x
-- pushMergeSub (ESubs (ESubs x s1) s2) =
--   let s = mergeSub s1 s2
--   in pushMergeSub (ESubs x s)
-- pushMergeSub x = x

-- -- Perform beta application
-- beta :: ETerm -> ETerm -> ETerm
-- beta body arg = ESubs body (\i -> if i == 0 then arg else EVar (i - 1))