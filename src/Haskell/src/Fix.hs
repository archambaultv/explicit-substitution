{-# LANGUAGE PatternSynonyms #-}

module Fix (
  Fix(..),
  CFix,
  Ann,
  Alg,
  CoAlg,
  pattern CAnn,
  pattern Ann,
  unAnn,
  removeAnn
) where

import Data.Fix (Fix(..))
import Data.Functor.Foldable (cata)
import Data.Functor.Compose (Compose(..))

type CFix w f = Fix (Compose w f)
type Ann b = ((,) b) -- Annotation
type Alg f a = f a -> a
type CoAlg f a = a -> f a

-- Pattern for composed annotation
pattern CAnn :: b -> f (CFix (Ann b) f) -> CFix (Ann b) f
pattern CAnn w f = Fix (Compose (w, f))
{-# COMPLETE CAnn #-}

-- Pattern for annotation
pattern Ann :: b -> f a -> Compose (Ann b) f a
pattern Ann w f = Compose (w, f)
{-# COMPLETE Ann #-}

unAnn :: CFix (Ann a) f -> f (CFix (Ann a) f)
unAnn (CAnn _ f) = f

removeAnn :: (Functor f) => CFix (Ann a) f -> Fix f
removeAnn = cata go
  where go :: Alg (Compose (Ann a) f) (Fix f)
        go (Ann _ f) = Fix f