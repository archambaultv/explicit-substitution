{-# LANGUAGE MultiParamTypeClasses, RankNTypes, ScopedTypeVariables, FunctionalDependencies #-}

module Abstract.Binders (
  Binding,
  freevar,
  rename
) where

import Data.Set (Set)
import qualified Data.Set as S
import Data.Maybe (fromJust, isNothing, mapMaybe)
import Data.Functor.Foldable (ana, apo, cata)
import Data.Void (Void, vacuous)
import Data.Bifunctor (Bifunctor, first)

import Fixpoint
import Abstract.Ast

-- Functors with finite number of bindings
class Binding f v | f -> v where
  tovar :: f r -> Maybe v
  toleaf :: v -> f Void
  -- How to rename when going under a binder
  -- under gives a function f : v -> Maybe v for each r
  -- s.t. f v = Just v' means the variable v is renamed v' when
  -- going under f. f v = Nothing means that it is not possible to look for
  -- a variable v under f (shadowing).
  -- All elements in the set will have an ouput value (Just v') for the function f : v -> Maybe r
  under :: Set v -> f r -> f (v -> Maybe v, r)
  -- over is the reverse of under,
  -- if under gives a function f : v -> Maybe v for each r,
  -- over gives the function g : v -> Maybe v such that
  -- g s v' = Just v -> f s v = Just v'
  -- g s v = Nothing -> ∄ v' s.t. f s v' = Just v
  -- All elements in the set will have an ouput value (Just v') for the function f : v -> Maybe r
  over :: Set v -> f r -> f (v -> Maybe v, r)

freevar :: forall f v. (Functor f, Binding f v, Foldable f, Ord v) => Fix f -> Set v
freevar = cata go
  where
  go :: Alg f (Set v)
  go x = case tovar x of
          Just v' -> S.singleton v'
          Nothing -> let b :: f (Set v)
                         b = fmap update $ over S.empty x
                     in S.unions b

  update :: ((v -> Maybe v), Set v) -> Set v
  update (f, s) = S.fromList $ mapMaybe f $ S.toAscList s

rename :: forall f v. (Functor f, Binding f v) => (v -> v) -> Fix f -> Fix f
rename a b = ana go (a,b)
  where go :: CoAlg f (v -> v, Fix f)
        go (r, Fix x) =
          case tovar x of
            Just v' -> vacuous $ toleaf $ r v'
            Nothing -> 
              let x1 :: f (v -> Maybe v, Fix f)
                  x1 = under S.empty x
                  x2 :: f (v -> v, Fix f)
                  x2 = (fmap . first) (merge r) x1
              in x2

        merge :: (v -> v) -> (v -> Maybe v) -> (v -> v)
        merge s1 s2 v = maybe v s1 (s2 v)


-- substitute :: forall f v. (Functor f, Foldable f, Binding f v, Ord v) => (v -> Fix f, Set v) -> Fix f -> Fix f
-- substitute (s, fv) body = apo go (s, S.union fv (freevar body), body)
--   where
--   go :: CoAlgM f (Either (Fix f)) (v -> Fix f, Set v, Fix f)
--   go (s1, fv1, Fix body1) = 
--     case tovar body1 of
--       Just v' -> fmap Left $ unFix $ s1 v'
--       Nothing -> 
--         let body2 :: f (v -> Maybe v, Fix f)
--             body2 = under fv1 body1

--             v -> Maybe (Fix f)
--             case f v of
--               Nothing -> Left t
--               Just v -> Right (s1 . (\x if x == v' then v' else v)) v -> Fix f, Set v, Fix f)

--             body3 :: f (Maybe v, Fix f)
--             body3 = fmap (\(f, t) -> (f v1, t)) body2
--             body4 :: f (Either (Fix f) (v, Fix f, Set v, Fix f))
--             body4 = fmap (update s1 fv1) body3
--         in body4

--   update :: (v -> Fix f) -> (Set v) -> (Maybe v, Fix f) -> (v, Fix f, Set v, Fix f)
--   update _ _ Nothing t = Left t
--   update oldv fv (Just newv) t = 
--     let fv' = S.map (\x -> x == oldv then newv else x) fv
--     in (newv, fv', t)




-- -- A term is a bindingConstruct if it introduces a binder
-- -- amoung its children
-- -- We don't use mempty of monoïd to avoid Sum Int in the case of deBruijn
-- -- Obviously any monoïd can supply the default binder value
-- bindingConstruct :: forall f b r. (Foldable f, Eq b) =>
--   Binders f b -> -- How to compute the binders
--   b -> -- The default value when no binder is used
--   f r -> -- The term
--   Bool
-- bindingConstruct binders defaultB x = foldr go False (binders x)
--   where go :: (b, r) -> Bool -> Bool
--         go (b, _) acc = b /= defaultB || acc


-- -- Renaming change a leaf by a leaf
-- renameSimple :: forall f n a. (Functor f, ToAst f n a) =>
--   (a -> a) -> -- Renaming
--   Fix f -> -- The term
--   Fix f
-- renameSimple r = ana go
--   where go :: CoAlg f (Fix f)
--         go (Fix x) = 
--           let ast :: NFix (AstF n a) (Fix f)
--               ast = toAst x
--           in case ast of
--                N1 (LeafF y) -> fromAst' $ N1 $ LeafF $ r y
--                _ -> x

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
