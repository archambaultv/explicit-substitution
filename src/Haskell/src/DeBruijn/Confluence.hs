module DeBruijn.Confluence
    ( confluence
    ) where

import DeBruijn.Terms
import DeBruijn.SmallStep (SmallSteps)

-- Confluence
-- The two steps must start with the same term
-- Both output have the same final term
confluence :: LTerm -> SmallSteps -> SmallSteps -> (SmallSteps, SmallSteps)
confluence _ _ _ = undefined