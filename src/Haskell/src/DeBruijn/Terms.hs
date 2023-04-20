{-# LANGUAGE DeriveTraversable, TemplateHaskell #-}

module DeBruijn.Terms
    ( TermF(..),
      Term,
      pattern TVar,
      pattern TAbs,
      pattern TApp,
      Renaming,
      Substitution,
      SubstitutionAttr,
      binders,
      coAlgBinders,
      rename,
      substitute,
      betaPattern,
      betaPatternFix,
      betaPatternAttr,
      applyBeta
    ) where

import Data.Monoid (Sum(..))
import Data.Group (invert)
import Text.Show.Deriving (deriveShow1)
import Data.Eq.Deriving (deriveEq1)
import Data.Functor.Foldable (hylo)
import Data.Void (vacuous)

import Attribute

data TermF r = TVarF Int
             | TAbsF r
             | TAppF r r
  deriving (Show, Eq, Functor, Foldable, Traversable)

$(deriveShow1 ''TermF)
$(deriveEq1 ''TermF)

instance HasLeaves TermF where
  type Leaf TermF = Sum Int
  fromLeaf n = TVarF (getSum n)
  isLeaf (TVarF n) = Just (Sum n)
  isLeaf _ = Nothing

type Term = Fix TermF

pattern TVar :: Int -> Term
pattern TVar x = Fix (TVarF x)

pattern TAbs :: Term -> Term
pattern TAbs x = Fix (TAbsF x)

pattern TApp :: Term -> Term -> Term
pattern TApp e1 e2 = Fix (TAppF e1 e2)

-- How many binders a term introduce
binders :: TermF r -> TermF (Sum Int, r)
binders (TAbsF r) = TAbsF (1, r)
binders x = fmap (0,) x

rename :: (Sum Int -> Sum Int) -> Term -> Term
rename = renameDeBruijn binders

substitute :: (Sum Int -> Term) -> Term -> Term
substitute = substituteDeBruijn binders

-- beta pattern
applyBeta :: Term -> Term
applyBeta (TApp (TAbs t1) t2) = substitute (\n -> if n == 0 then t2 else Fix $ vacuous $ fromLeaf (n <> invert 1)) t1
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