module Fix (
  CFix,
  Ann,
  Alg
) where

import Data.Fix (Fix(..))
import Data.Functor.Compose (Compose(..))

type CFix w f = Fix (Compose w f)
type Ann b = ((,) b) -- Annotation
type Alg f a = f a -> a