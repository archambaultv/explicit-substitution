{-# LANGUAGE DeriveTraversable, ScopedTypeVariables, MultiParamTypeClasses #-}

module Abstract.AST (
  AstF(..),
  Ast,
  pattern Leaf,
  pattern Node,
  ToAst(..),
  mapLeaf,
  mapNode,
  AstPat,
  astMatch
) where

import Data.Functor.Foldable (hylo)

import Fixpoint

-- This is basically an SExpression
data AstF n a r = LeafF a
                | NodeF n [r]
    deriving (Eq, Show, Functor, Foldable, Traversable)

type Ast n a = Fix (AstF n a)

class ToAst f n a where
  toAst :: f r -> NFix (AstF n a) r

-- Pattern for composed annotation
pattern Leaf :: a -> Ast n a
pattern Leaf a = Fix (LeafF a)

-- Pattern for annotation
pattern Node :: n -> [Ast n a] -> Ast n a
pattern Node n ns = Fix (NodeF n ns)
{-# COMPLETE Leaf, Node #-}

-- Applies the change only if f is a leaf
mapLeaf :: (a -> b) -> AstF n a r -> AstF n b r
mapLeaf f (LeafF x) = LeafF (f x)
mapLeaf _ (NodeF n rs) = (NodeF n rs)

-- Applies the change only if f is a leaf
mapNode :: (n1 -> n2) -> AstF n1 a r -> AstF n2 a r
mapNode f (NodeF n rs) = NodeF (f n) rs
mapNode _ (LeafF x) = (LeafF x)

type AstPat n a = CFix Maybe (AstF (n -> Bool) (a -> Bool))

astMatch :: AstPat n a -> Ast n a -> Bool
astMatch pat t = hylo alg coAlg (t, pat)
  where coAlg :: CoAlg (AstF Bool Bool) (Ast n a, AstPat n a)
        coAlg (_, Fix (Compose Nothing)) = LeafF True
        coAlg (Leaf x, Fix (Compose (Just (LeafF assert)))) = LeafF (assert x)
        coAlg (Node x xs, Fix (Compose (Just (NodeF assert pats)))) = 
          let match = assert x
              sameLength = length xs == length pats
              next = zip xs pats
          in NodeF (match && sameLength) next
        coAlg (_,_) = LeafF False

        alg :: Alg (AstF Bool Bool) Bool
        alg (LeafF x) = x
        alg (NodeF x xs) = x && and xs