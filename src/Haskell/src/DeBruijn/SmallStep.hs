module DeBruijn.SmallStep
    ( ParStep,
      ParSteps,
      reduce,
      allPossibleSteps,
      reduceMany,
      validParSteps
    ) where

import Data.Functor.Foldable (ListF(..), cata, para)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE

import Fix
import DeBruijn.Terms

-- Small steps and parallel small steps
-- We simply can annotate each node with False if we don't or can't perform beta
-- reduction and True if we should perform beta reduction.
-- The classic small step relation is when there is one and only one True
-- With multiple True or with only False, this is the (reflexive) parallel small step relation
type ParStep = CFix (Ann Bool) TermF

reduce :: ParStep -> Term
reduce (CAnn True (TAppF (CAnn _ (TAbsF t1)) t2)) = 
  let t1' = reduce t1
      t2' = reduce t2
  in substitute (\n -> if n == 0 then t2' else TVar (n - 1)) t1'
reduce (CAnn _ x) = Fix $ fmap reduce x

allPossibleSteps :: Term -> ParStep
allPossibleSteps = cata go
  where go :: Alg TermF ParStep
        go x = if betaPattern (fmap unAnn x)
               then CAnn True x
               else CAnn False x

-- Kleene closure 
-- Of course we expect that the nth term in the sequence has the same shape of
-- the reduced (n-1)th term. 
-- Note that many classic small step can be encoded within a single ParStep
type ParSteps = NonEmpty ParStep

reduceMany :: ParSteps -> Term
reduceMany xs = reduce $ NE.last xs

validParSteps :: ParSteps -> Bool
validParSteps (x :| xs) = para go (x : xs)
  where go :: ListF ParStep ([ParStep], Bool) -> Bool
        go Nil = True
        go (Cons _ ([], valid)) = valid
        go (Cons a ((b:_), valid)) =
          if reduce a == removeAnn b
          then valid
          else False