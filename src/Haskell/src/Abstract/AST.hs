{-# LANGUAGE DeriveTraversable, ScopedTypeVariables, MultiParamTypeClasses #-}

module Abstract.AST (
  AstF(..),
  Ast,
  ToAst(..),
) where

import Data.Bifunctor.TH
import Data.Bifunctor (first)
import Data.Functor.Foldable (ana, hylo)

import Fixpoint

-- This is basically an SExpression
data AstF n a r = LeafF a
                | NodeF n [r]
    deriving (Eq, Show, Functor, Foldable, Traversable)

type Ast n a = Fix (AstF n a)

class ToAst f n a where
  toAst :: f r -> NFix (AstF n a) r

-- -- -- Applies the change only if f is a leaf
-- -- changeLeaf :: (AST f) => (Leaf f -> Leaf f) -> f r -> f r
-- -- changeLeaf f x = 
-- --   case toTree x of
-- --     NodeF l [] -> fromTree $ NodeF (fromLeaf $ f $ toLeaf l) []
-- --     _ -> x

-- toAST :: (AST f) => Fix f -> Tree (Label f)
-- toAST = ana (natTransCoAlg toTree)

-- fromAST :: (AST f, Functor f) => Tree (Label f) -> Fix f
-- fromAST = ana (natTransCoAlg fromTree)

-- isLeaf :: (AST f) => f r -> Bool
-- isLeaf = null . subForestF . toTree

-- type ASTPat f = CFix Maybe (TreeF (Label f -> Bool))

-- fpatMatch :: forall f. (AST f) => ASTPat f -> Fix f -> Bool
-- fpatMatch pat t = hylo alg coAlg (t, pat)
--   where coAlg :: CoAlg (TreeF Bool) (Fix f, ASTPat f)
--         coAlg (_, Fix (Compose Nothing)) = NodeF True []
--         coAlg (x, Fix (Compose (Just (NodeF criteria pats)))) =
--           let (NodeF n xs) = toTree (unFix x)
--               match = criteria n
--               sameLength = length xs == length pats
--               next = zip xs pats
--           in NodeF (match && sameLength) next

--         alg :: Alg (TreeF Bool) Bool
--         alg (NodeF x xs) = x && and xs