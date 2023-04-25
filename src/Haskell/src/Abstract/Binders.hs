{-# LANGUAGE RankNTypes, ScopedTypeVariables, FlexibleInstances, FlexibleContexts #-}

module Abstract.Binders (
  -- Binders,
  -- bindingConstruct,
  -- HasLeaves(..),
  -- renameSimple,
  -- renameDeBruijn,
  -- substituteSimple,
  -- substituteDeBruijn
) where

-- import Data.Functor.Foldable (ana, apo)
-- import Data.Void (Void, vacuous)
-- import Data.Bifunctor (first)

-- import Fixpoint
-- import Abstract.AST

-- -- Binders
-- type Binders f b = forall r. f r -> f (b, r)

-- -- A term is a bindingConstruct if it introduces a binder
-- -- amoung its children
-- bindingConstruct :: forall f b r. (Foldable f, Eq b) =>
--   Binders f b -> -- How to compute the binders
--   b -> -- The default value when no binder is used
--   f r -> -- The term
--   Bool
-- bindingConstruct binders defaultB x = foldr go False (binders x)
--   where go :: (b, r) -> Bool -> Bool
--         go (b, _) acc = b /= defaultB || acc


-- -- Renaming change a leaf by a leaf
-- renameSimple :: forall f. (Functor f, AST f) =>
--   (Leaf f -> Leaf f) -> -- Renaming
--   Fix f -> -- The term
--   Fix f
-- renameSimple r = ana go
--   where go :: CoAlg f (Fix f)
--         go (Fix x) = changeLeaf x

-- -- We use the attribute to indicate how many binders are above
-- renameDeBruijn :: forall f. (Functor f, AST f, Leaf f ~ Int) =>
--   Binders f Int -> -- How to count the binders
--   (Int -> Int) -> -- How to rename
--   Fix f -> -- The term
--   Fix f -- The result
-- renameDeBruijn binders r t = ana go (0, t)
--   where go :: CoAlg f (Int, Fix f)
--         go (n, Fix x) =
--           if isLeaf x
--           then changeLeaf $ \l -> if l < n then l else r (l - n) + n
--           else first (+ n) <$> binders x

-- -- Substitution
-- -- Change a leaf by a term
-- substituteSimple :: forall f. (Functor f, HasLeaves f) =>
--   (Leaf f -> Fix f) ->
--   Fix f ->
--   Fix f
-- substituteSimple s = ana go
--   where go :: CoAlg f (Fix f)
--         go (Fix x) = maybe x (unFix . s) (isLeaf x)

-- substituteDeBruijn :: forall f. (Functor f, HasLeaves f, Leaf f ~ Int) =>
--   Binders f Int -> -- How to count the binders
--   (Int -> Fix f) -> -- How to substitute
--   Fix f -> -- The body
--   Fix f -- The result
-- substituteDeBruijn binders s t = apo go (0, t)
--   where go :: CoAlgM f (Either (Fix f)) (Int, Fix f)
--         go (n, Fix x) =
--           case isLeaf x of
--             Just l -> Left <$>
--                       if l < n
--                       then vacuous $ fromLeaf l
--                       else unFix $ renameDeBruijn binders (+ n) $ s (l - n)
--             Nothing -> Right . first (+ n) <$> binders x
