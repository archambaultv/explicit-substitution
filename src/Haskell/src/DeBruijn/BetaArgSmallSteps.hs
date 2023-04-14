module DeBruijn.BetaArgSmallSteps
    ( SmallStep(..),
      SmallSteps,
      reduce
    ) where

import DeBruijn.Terms

-- A new step, the possibility of first making a step in the argument and then
-- apply beta reduction
data SmallStep = SSAbs SmallStep
             | SSAppL SmallStep 
             | SSAppR SmallStep
             | SSBeta
             | SSBetaArg SmallStep
             deriving (Show, Eq)

-- Kleene closure
type SmallSteps = [SmallStep] 

reduce :: LTerm -> SmallStep -> LTerm
reduce (LAbs x) (SSAbs s) = LAbs $ reduce x s
reduce (LApp x y) (SSAppL s) = LApp (reduce x s) y
reduce (LApp x y) (SSAppR s) = LApp x (reduce y s)
reduce (LApp (LAbs body) arg) SSBeta = subst body arg
reduce (LApp (LAbs body) arg) (SSBetaArg s) = subst body (reduce arg s)
reduce _ _ = error "Invalid arguments"

-- diamond :: SSLTermB -> SSLTermB -> (SSLTermB, SSLTermB)
-- diamond x y = if checkValidity x y then diamond' x y else error "Invalid arguments"
--   where checkValidity x y = preStepTerm x == preStepTerm y

-- diamond' (SSAbs x) (SSAbs y) = bimap SSAbs SSAbs (diamond' x y)
-- diamond' (SSAppL x1 y1) (SSAppL x2 _) = 
--   let f = (\z -> SSAppL z y1)
--   in bimap f f (diamond' x1 x2)
-- diamond' (SSAppL x1 y1) (SSAppR x2 y2) = 
--   let l = postStepTerm x1
--       r = postStepTerm y2
--   in (SSAppR l y2, SSAppL x1 r)
-- diamond' (SSAppLB (SSAbs abs) arg) (SSBetaB body _) =
--   let body1 = postStepTerm abs
--       body2 = subst body arg
--   in (SSBeta body1 arg, abs)

-- diamond' (SSAppRB x1 y1) (SSAppLB x2 y2) =
--   let (a, b) = diamond' (SSAppLB x2 y2) (SSAppRB x1 y1)
--   in (b, a)
-- diamond' (SSAppRB x1 y1) (SSAppRB _ y2) = 
--   let f = (\z -> SSAppRB x1 z)
--   in bimap f f (diamond' y1 y2)
-- diamond' _ _ = error "Invalid arguments"