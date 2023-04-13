module LambdaCalculus
    ( LTerm(..),
      shift,
      subst,
      beta
    ) where

-- Standard definition
data LTerm = LVar Int
           | LAbs LTerm
           | LApp LTerm LTerm
           deriving (Show, Eq)

-- shift n t adds n to all the free variables in t
shift :: Int -> LTerm -> LTerm
shift n t = shift' 0 n t

shift' :: Int -> Int -> LTerm -> LTerm
shift' p _ (LVar i) | i < p = LVar i
shift' _ n (LVar i) = LVar (i + n)
shift' p n (LAbs t) = LAbs (shift' (p + 1) n t)
shift' p n (LApp t1 t2) = LApp (shift' p n t1) (shift' p n t2)

-- subst arg t replaces the free variable #0 of t by arg.
subst :: LTerm -> LTerm -> LTerm
subst arg t = subst' 0 arg t

subst' :: Int -> LTerm -> LTerm -> LTerm
subst' p _ (LVar i) | i < p = LVar i
subst' p arg (LVar i) | i == p = shift p arg
subst' _ _ (LVar i)  = LVar (i - 1)
subst' p arg (LAbs t) = LAbs (subst' (p + 1) arg t)
subst' p arg (LApp t1 t2) = LApp (subst' p arg t1) (subst' p arg t2)

-- Perform beta application
beta :: LTerm -> LTerm -> LTerm
beta body arg = subst arg body