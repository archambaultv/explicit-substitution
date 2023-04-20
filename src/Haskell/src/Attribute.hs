{-# LANGUAGE RankNTypes, ScopedTypeVariables, FlexibleInstances, FlexibleContexts #-}

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
  NatTrans,
  natTransAlg,
  natTransCoAlg,
  bottomUpAttr,
  topDownAttr,
  HasLeaves(..),
  renameSimple,
  renameDeBruijn,
  substituteSimple,
  substituteDeBruijn
) where

import Data.Fix (Fix(..))
import Data.Group (Group(..))
import Data.Functor.Foldable (cata, ana, apo)
import Data.Functor.Compose (Compose(..))
import Data.Void (Void, vacuous)
import Data.Bifunctor (first)

type Alg f a = f a -> a
type CoAlg f a = a -> f a
type CoAlgM f m a = a -> f (m a)

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

-- Natural transformations
type NatTrans f g = forall r. f r -> g r

natTransAlg :: NatTrans f g -> Alg f (Fix g)
natTransAlg nt f = Fix $ nt f

natTransCoAlg :: NatTrans f g -> CoAlg g (Fix f)
natTransCoAlg nt f = nt $ unFix f

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

-- Binders
type Binders f b = forall r. f r -> f (b, r)

class HasLeaves f where
  type Leaf f
  fromLeaf :: Leaf f -> f Void
  isLeaf :: forall r. f r -> Maybe (Leaf f)

instance (HasLeaves f) => HasLeaves (Compose ((,) b) f) where
  type Leaf (AttrF b f) = (b, Leaf f)
  fromLeaf (b, x) = AttrF b (fromLeaf x)
  isLeaf (AttrF b x) = (b,) <$> isLeaf x


-- Renaming change a leaf by a leaf
-- Conceptually, it has the type
-- type Renaming f = Leaf f -> Leaf f

renameSimple :: forall f. (Functor f, HasLeaves f) =>
  (Leaf f -> Leaf f) -> -- Renaming
  Fix f -> -- The term
  Fix f
renameSimple r = ana go
  where go :: CoAlg f (Fix f)
        go (Fix x) =
          case isLeaf x of
            Just l -> vacuous $ fromLeaf $ r l
            Nothing -> x

-- We use the attribute to indicate how many binders are above
renameDeBruijn :: forall f. (Functor f, HasLeaves f, Group (Leaf f), Ord (Leaf f)) =>
  Binders f (Leaf f) -> -- How to count the binders
  (Leaf f -> Leaf f) -> -- How to rename
  Fix f -> -- The term
  Fix f -- The result
renameDeBruijn binders r t = ana go (mempty, t)
  where go :: CoAlg f (Leaf f, Fix f)
        go (n, Fix x) =
          case isLeaf x of
            Just l -> vacuous $ fromLeaf $ if l < n then l else r (l <> invert n) <> n
            Nothing -> first (<> n) <$> binders x

-- Change a leaf by a term
substituteSimple :: forall f. (Functor f, HasLeaves f) => 
  (Leaf f -> Fix f) -> 
  Fix f -> 
  Fix f
substituteSimple s = ana go
  where go :: CoAlg f (Fix f)
        go (Fix x) = maybe x (unFix . s) (isLeaf x)

substituteDeBruijn :: forall f. (Functor f, HasLeaves f, Group (Leaf f), Ord (Leaf f)) =>
  Binders f (Leaf f) -> -- How to count the binders
  (Leaf f -> Fix f) -> -- How to substitute
  Fix f -> -- The term
  Fix f -- The result
substituteDeBruijn binders s t = apo go (mempty, t)
  where go :: CoAlgM f (Either (Fix f)) (Leaf f, Fix f)
        go (n, Fix x) =
          case isLeaf x of
            Just l -> Left <$> 
                      if l < n
                      then vacuous $ fromLeaf l
                      else unFix $ renameDeBruijn binders (<> n) $ s (l <> invert n)
            Nothing -> Right . first (<> n) <$> binders x
