{-# LANGUAGE RankNTypes, ScopedTypeVariables, FlexibleInstances, FlexibleContexts #-}

module Fixpoint.Attribute (
  Compose(..),
  CFix,
  Attr,
  AttrF,
  pattern Attr,
  pattern AttrF,
  unAttr,
  removeAttr,
  withAttr,
  bottomUpAttr,
  topDownAttr
) where


import Data.Functor.Foldable (cata, ana)
import Data.Functor.Compose (Compose(..))

import Fixpoint.Definitions

type CFix w f = Fix (Compose w f)
type Attr b f = CFix ((,) b) f -- Attribute or attributed
type AttrF b f = Compose ((,) b) f

-- Pattern for composed annotation
pattern Attr :: b -> f (Attr b f) -> Attr b f
pattern Attr w f = Fix (Compose (w, f))
{-# COMPLETE Attr #-}

-- Pattern for annotation
pattern AttrF :: b -> f a -> Compose ((,) b) f a
pattern AttrF w f = Compose (w, f)
{-# COMPLETE AttrF #-}

getAttr :: Attr a f -> a
getAttr (Attr a _) = a

unAttr :: Attr a f -> f (Attr a f)
unAttr (Attr _ f) = f

removeAttr :: (Functor f) => Attr a f -> Fix f
removeAttr = cata go
  where go :: Alg (AttrF a f) (Fix f)
        go (AttrF _ f) = Fix f

-- Annotate a term with attributes
withAttr :: forall f b. (Functor f) => 
  NatTransK f b ->
  Fix f ->
  Attr b f
withAttr nt = cata go
  where go :: Alg f (Attr b f)
        go x = Attr (nt x) x

-- Accumulate attributes
bottomUpAttr :: forall b f. (Monoid b, Functor f, Foldable f) => Attr b f -> Attr b f
bottomUpAttr = cata go
  where go :: Alg (AttrF b f) (Attr b f)
        go (AttrF b f) =
          let b' = b <> foldMap getAttr f
          in Attr b' f

topDownAttr :: forall b f. (Monoid b, Functor f) => Attr b f -> Attr b f
topDownAttr x = ana go (x, mempty)
  where go :: CoAlg (AttrF b f) (Attr b f, b)
        go (Attr b f, b') =
          let b2 = b' <> b
          in AttrF b2 $ fmap (, b2) f