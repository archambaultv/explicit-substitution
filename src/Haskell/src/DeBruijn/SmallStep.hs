module DeBruijn.SmallStep
    ( SmallStep(..),
      SmallSteps,
      reduce,
      allPossibleSteps
    ) where

import DeBruijn.Terms

-- small steps
-- This is basically just a path to indicate where to perform a beta reduction
data SmallStep = SSAbs SmallStep
               | SSAppL SmallStep
               | SSAppR SmallStep
               | SSBeta
               deriving (Show, Eq)

-- Kleene closure
type SmallSteps = [SmallStep] 

reduce :: LTerm -> SmallStep -> LTerm
reduce (LAbs x) (SSAbs s) = LAbs $ reduce x s
reduce (LApp x y) (SSAppL s) = LApp (reduce x s) y
reduce (LApp x y) (SSAppR s) = LApp x (reduce y s)
reduce (LApp (LAbs body) arg) SSBeta = subst body arg
reduce _ _ = error "Invalid arguments"

allPossibleSteps :: LTerm -> [SmallStep]
allPossibleSteps (LVar _) = []
allPossibleSteps (LAbs t) = map SSAbs $ allPossibleSteps t
allPossibleSteps (LApp t1 t2) = 
  map SSAppL (allPossibleSteps t1) ++
  map SSAppR (allPossibleSteps t2)