module DeBruijn.Terms
    ( LTerm(..),
      shift,
      subst
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

-- subst body arg replaces the free variable #0 of body by arg.
subst :: LTerm -> LTerm -> LTerm
subst body arg = subst' 0 body arg

subst' :: Int -> LTerm -> LTerm -> LTerm
subst' p (LVar i) _ | i < p = LVar i
subst' p (LVar i) arg | i == p = shift p arg
subst' _ (LVar i) _ = LVar (i - 1)
subst' p (LAbs t) arg = LAbs (subst' (p + 1) arg t)
subst' p (LApp t1 t2) arg = LApp (subst' p arg t1) (subst' p arg t2)