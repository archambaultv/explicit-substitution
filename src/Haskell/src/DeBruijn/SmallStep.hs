module DeBruijn.SmallStep
    ( 
      -- ParStep,
      -- ParStepF,
      -- ParSteps,
      -- reduce,
      -- allPossibleSteps,
      -- reduceMany,
      -- validParSteps
    ) where

-- import Data.Functor.Foldable (ListF(..), cata, para)
-- import Data.List.NonEmpty (NonEmpty(..))
-- import qualified Data.List.NonEmpty as NE

-- import Fixpoint
-- import DeBruijn.Terms

-- -- Small steps and parallel small steps
-- -- We simply can annotate each node with False if we don't or can't perform beta
-- -- reduction and True if we should perform beta reduction.
-- -- The classic small step relation is when there is one and only one True
-- -- With multiple True or with only False, this is the (reflexive) parallel small step relation
-- type ParStep = Attr Bool TermF
-- type ParStepF = AttrF Bool TermF

-- reduce :: ParStep -> Term
-- reduce = cata go
--   where go :: Alg ParStepF Term
--         go (AttrF isStep t) =
--           if isStep && betaPattern (fmap unFix t)
--           then applyBeta $ Fix t
--           else Fix t

-- allPossibleSteps :: Term -> ParStep
-- allPossibleSteps = cata go
--   where go :: Alg TermF ParStep
--         go x = if betaPattern (fmap unAttr x)
--                then Attr True x
--                else Attr False x

-- -- Kleene closure 
-- -- Of course we expect that the nth term in the sequence has the same shape of
-- -- the reduced (n-1)th term. 
-- -- Note that many classic small step can be encoded within a single ParStep
-- type ParSteps = NonEmpty ParStep

-- reduceMany :: ParSteps -> Term
-- reduceMany xs = reduce $ NE.last xs

-- validParSteps :: ParSteps -> Bool
-- validParSteps (x :| xs) = para go (x : xs)
--   where go :: ListF ParStep ([ParStep], Bool) -> Bool
--         go Nil = True
--         go (Cons _ ([], valid)) = valid
--         go (Cons a ((b:_), valid)) =
--           if reduce a == removeAttr b
--           then valid
--           else False