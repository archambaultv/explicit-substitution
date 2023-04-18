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
-- we can define another parstep p2 such that:
--    reducyMany [p1, p2] = reduce (triop t)
-- triangle :: ParStep -> ParStep
-- triangle s =
--   let t = removeAnn s


-- confluence :: LTerm -> SmallSteps -> SmallSteps -> (SmallSteps, SmallSteps)
-- confluence _ _ _ = undefined