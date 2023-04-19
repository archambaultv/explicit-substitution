module DeBruijn.Confluence
    ( triop
    ) where

import DeBruijn.Terms
import DeBruijn.SmallStep


-- Triangle operator
triop :: Term -> ParStep
triop = allPossibleSteps

-- triangle property
-- Given a parstep p1 of term t, 
-- we can define another parstep p2 = triangle p1 such that:
--    reduceMany [p1, p2] = reduce (triop t)
triangle :: ParStep -> ParStep
triangle = cata go 
  where go :: Alg ParStepF ParStep
        go (Ann isStep t) = 
          case (isStep, betaPattern (fmap unAnn t))
            (_, False) -> CAnn False t
            (False, True) -> CAnn True t
            (True, True) -> applyBeta $ Fix t


-- confluence :: LTerm -> SmallSteps -> SmallSteps -> (SmallSteps, SmallSteps)
-- confluence _ _ _ = undefined