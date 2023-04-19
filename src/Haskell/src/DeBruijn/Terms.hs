{-# LANGUAGE DeriveTraversable, TemplateHaskell, PatternSynonyms, TupleSections #-}

module DeBruijn.Terms
    ( TermF(..),
      Term,
      pattern TVar,
      pattern TAbs,
      pattern TApp,
      Renaming,
      Substitution,
      SubstitutionAttr,
      countBinders,
      coAlgBinders,
      rename,
      substitute,
      betaPattern,
      betaPatternFix,
      betaPatternAttr,
      applyBeta
    ) where

import Text.Show.Deriving (deriveShow1)
import Data.Eq.Deriving (deriveEq1)
import Data.Functor.Foldable (hylo)

import Attribute

data TermF r = TVarF Int
             | TAbsF r
             | TAppF r r
  deriving (Show, Eq, Functor, Foldable, Traversable)

$(deriveShow1 ''TermF)
$(deriveEq1 ''TermF)

type Term = Fix TermF
type Renaming = Int -> Int
type Substitution = Int -> Term
type SubstitutionAttr b = Int -> Attr b TermF

pattern TVar :: Int -> Term
pattern TVar x = Fix (TVarF x)

pattern TAbs :: Term -> Term
pattern TAbs x = Fix (TAbsF x)

pattern TApp :: Term -> Term -> Term
pattern TApp e1 e2 = Fix (TAppF e1 e2)

-- How many binders a term introduce
countBinders :: TermF r -> Int
countBinders (TAbsF _) = 1
countBinders _ = 0

-- A coAlgebra to count binders. To use for example in a hylomorphism
coAlgBinders :: CoAlg (AttrF Int TermF) (Term, Int)
coAlgBinders (Fix x, n) =
  let n' = countBinders x + n
  in AttrF n' (fmap (,n') x)

rename :: Renaming -> Term -> Term
rename r t = hylo go coAlgBinders (t, 0)
  where go :: Alg (AttrF Int TermF) Term
        go (AttrF n (TVarF i)) = if i < n then TVar i else TVar $ r (i - n) + n
        go (AttrF _ x) = Fix x

substitute :: Substitution -> Term -> Term
substitute s t = hylo go coAlgBinders (t, 0)
  where go :: Alg (AttrF Int TermF) Term
        go (AttrF n (TVarF i)) = rename (+ n) $ s (i - n)
        go (AttrF _ x) = Fix x

-- beta pattern
applyBeta :: Term -> Term
applyBeta (TApp (TAbs t1) t2) = substitute (\n -> if n == 0 then t2 else TVar (n - 1)) t1
applyBeta x =
  let x' :: TermF (TermF String)
      x' = (fmap (const "") . unFix <$> unFix x)
  in error $ "Cannot apply beta reduction on term " ++ show x'

betaPattern :: TermF (TermF r) -> Bool
betaPattern (TAppF (TAbsF _) _) = True
betaPattern _ = False

betaPatternFix :: Term -> Bool
betaPatternFix x = betaPattern (unFix <$> unFix x)

betaPatternAttr :: Attr b TermF -> Bool
betaPatternAttr x = betaPattern (unAttr <$> unAttr x)