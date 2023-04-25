{-# LANGUAGE TypeOperators, FlexibleInstances, FlexibleContexts, 
    UndecidableInstances, MultiParamTypeClasses, ScopedTypeVariables, InstanceSigs #-}

-- Poly functors can be converted to AST
module Abstract.Polyfunctors (
  I(..),
  K(..),
  (:+:)(..),
  (:*:)(..),
  PolyLabel(..)
) where

import Fixpoint
import Abstract.AST

-- infixl 6 :+:
-- infixl 7 :*:
-- infixr 9 :.:

-- data PolyTypeF a r = K a
--                    | I
--                    | (:+:) r r
--                    | (:*:) r r

-- type PolyType a = Fix (PolyTypeF a)

-- data PolyTermF a r = TK a
--                    | InL r
--                    | InR r
--                    | Tup r r

-- type PolyTerm a = Fix (PolyTermF a)

-- deBruijnType :: PolyType Int
-- deBruijnType = Fix $ K 3


newtype I a = I a

instance Functor I where
    fmap f (I x) = I (f x)

newtype K c a = K c
instance Functor (K c) where
    fmap _ (K c) = K c

infixl 6 :+:
data (f :+: g) a = InL (f a) | InR (g a)
instance (Functor f, Functor g) => Functor (f :+: g) where
    fmap f (InL fx) = InL (fmap f fx)
    fmap f (InR gy) = InR (fmap f gy)

infixl 7 :*:
data (f :*: g) a = f a :*: g a
instance (Functor f, Functor g) => Functor (f :*: g) where
    fmap f (fx :*: gy) = fmap f fx :*: fmap f gy

-- infixr 9 :.:
-- newtype (f :.: g) a = Cmp (f (g a))
-- instance (Functor f, Functor g) => Functor (f :.: g) where
--     fmap f (Cmp fgx) = Cmp (fmap (fmap f) fgx)

data PolyLabel = Id -- Identity
               | L -- Left
               | R -- Right
               | P -- Product
              -- | C -- Compose

instance ToAst (K c) PolyLabel c where
  toAst (K c) = N1 $ LeafF c

instance ToAst I PolyLabel c where
  toAst (I x) = N1 $ NodeF Id [x]

instance (ToAst f PolyLabel c, ToAst g PolyLabel c) => ToAst (f :+: g) PolyLabel c where
  toAst (InL x) = 
    let l' = toAst x
    in case l' of
        N1 f' -> N2 $ NodeF L [f']
        N2 _ -> error "Limited to two level"
  toAst (InR x) =
    let r' = toAst x
    in case r' of
        N1 g' -> N2 $ NodeF R [g']
        _ -> error "Limited to two level"

instance (ToAst f PolyLabel c, ToAst g PolyLabel c) => ToAst (f :*: g) PolyLabel c where
  toAst (x1 :*: x2) = 
    let x1' = toAst x1
        x2' = toAst x2
    in case (x1', x2') of
        (N1 f', N1 g') -> N2 $ NodeF P [f', g']
        _ -> error "Limited to two level"