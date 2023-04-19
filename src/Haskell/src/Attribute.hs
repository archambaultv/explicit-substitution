{-# LANGUAGE PatternSynonyms #-}

module Attribute (
  Fix(..),
  CFix,
  Attr,
  AttrF,
  Alg,
  CoAlg,
  pattern Attr,
  pattern AttrF,
  unAttr,
  removeAttr,
  toAttrAlg
) where

import Data.Fix (Fix(..))
import Data.Functor.Foldable (cata)
import Data.Functor.Compose (Compose(..))

type Alg f a = f a -> a
type CoAlg f a = a -> f a

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

unAttr :: Attr a f -> f (Attr a f)
unAttr (Attr _ f) = f

removeAttr :: (Functor f) => Attr a f -> Fix f
removeAttr = cata go
  where go :: Alg (AttrF a f) (Fix f)
        go (AttrF _ f) = Fix f


toAttrAlg :: Alg f a -> Alg (AttrF b f) a
toAttrAlg alg (AttrF _ x) = alg x

-- toAttrCoAlg :: CoAlg f (Fix a) -> CoAlg f (Attr b a)
-- toAttrCoAlg coAlg x@(Attr b _) = fmap (\x -> Attr b (unFix x)) 
--                              $ coAlg 
--                              $ removeAttr x 
