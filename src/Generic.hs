{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wincomplete-patterns#-}
module Generic where

------------------------------------

import Control.Applicative
import Control.Monad
import Control.Monad.Identity hiding (fix, mfix)
import Data.Foldable hiding (all, and, concat, concatMap, fold, foldl, toList)
import Data.Traversable hiding (mapM, sequence)
import Fix

------------------------------------

data Rec f a
  = Var a
  | Mu ([a] -> [f (Rec f a)])
  | In (f (Rec f a))

-- f has to be Functor, Foldable and Traversable.
newtype Graph f = Hide {reveal :: forall a . Rec f a}

-- This representation prevents empty cycles e.g. Mu (\v -> Var v)

-------------------------------------
-- Streams Revisited

data StreamF a r = Cons a r
  deriving (Functor, Foldable, Traversable)

type Stream a = Graph (StreamF a)

-- Cyclic stream (1 : 2 : ...)
-- Almost same representation as in PStream.
onetwo = Hide (Mu (\(~(s:_)) ->
  [Cons 1 (In (Cons 2 (Var s)))]))

-- ad-hoc, later we will see a better definition
toList' :: Stream a -> [a]
toList' = ptoS . reveal where
  ptoS   (Var x)     = x
  ptoS   (Mu g)      = head $ fix (map ptoS' . g)
  ptoS   (In r)      = ptoS' r
  ptoS'  (Cons x xs)  = x : ptoS xs

-------------------------------------
-- Tree Revisited

data TreeF a r = Empty | Fork a r r
  deriving (Functor, Foldable, Traversable)

type Tree a = Graph (TreeF a)

-- Cyclic Tree example:
tree =
  Hide
    ( Mu
        ( \(~(t1 : t2 : t3 : _)) ->
            [ Fork 1 (In (Fork 4 (Var t2) (In Empty))) (Var t3),
              Fork 2 (Var t1) (Var t3),
              Fork 3 (Var t2) (Var t1)
            ]
        )
    )

-------------------------------------
-- Generic Operations

-- 1. Graph-generic operations: abstracts over a functor f.
-- 2. Fixpoint-parametrized: gfold generalizes the treatment of recursive binders (mu).

gfold
  :: Functor f
  => (t -> c)            -- Applied to Var a
  -> (([t] -> [c]) -> c) -- Fixpoint-parametrized
  -> (f c -> c)          -- Actual fold
  -> Graph f
  -> c
gfold v l f = trans . reveal where
  trans (Var x)   = v x
  trans (Mu g)    = l (map (f . fmap trans) . g)
  trans (In fa)   = f (fmap trans fa)

-- Acyclic
fold
  :: Functor f
  => (f c -> c)
  -> c
  -> Graph f
  -> c
fold alg k = gfold id (\g -> head (g (repeat k))) alg

-- Cyclic
cfold
  :: Functor f
  => (f t -> t)
  -> Graph f
  -> t
cfold = gfold id (head . fix)

-- Uses a special fixpoint operation 'fixVal' which works for monotonic functions
-- and values that support a comparison operation (≣).
sfold
  :: (Eq t, Functor f)
  => (f t -> t)
  -> t
  -> Graph f
  -> t
sfold alg k = gfold id (head . fixVal (repeat k)) alg

fixVal :: Eq a => a -> (a -> a) -> a
fixVal v f = if v==v' then v else fixVal v' f
    where v' = f v

---------------------------------------
-- Examples of using fold, cfold, sfold

-- >>> elems onetwo
-- [1, 2]
elems :: Stream a -> [a]
elems = fold streamf2list []

-- >>> take 10 $ toList onetwo
-- [1,2,1,2,1,2,1,2,1,2]
toList :: Stream a -> [a]
toList = cfold streamf2list

streamf2list :: StreamF a [a] -> [a]
streamf2list (Cons x xs) = x : xs

---------------------------------------
-- Generalizing Transformations

-- natural transformation
type f ~> g = forall a . f a -> g a

transform
  :: forall f g. (Functor f, Functor g)
  => (f ~> g)
  -> Graph f
  -> Graph g
transform f x = Hide (hmap (reveal x)) where
   hmap :: forall a. Rec f a -> Rec g a
   hmap (Var x)    = Var x
   hmap (Mu g)     = Mu (map (f . fmap hmap) . g)
   hmap (In x)     = In (f (fmap hmap x))

-- Example of transform: from Tree to VGraph

data VGraphF a = VNode String [a]
  deriving (Show, Functor, Foldable, Traversable)

type VGraph = Graph VGraphF

btree2vgraph :: Show a => Tree a -> VGraph
btree2vgraph = transform trans where
  trans Empty         = VNode "" []
  trans (Fork x l r)  = VNode (show x) [l,r]

--------------------------------------
-- Generic Mapping: gmap

-- Requires f to be a Bifunctor (which it is always is)

class BiFunctor f where
  bimap :: (a -> c) -> (b -> d) -> f a b -> f c d

gmap
  :: (BiFunctor f, Functor (f a), Functor (f b))
  => (a -> b)
  -> Graph (f a)
  -> Graph (f b)
gmap f = transform (bimap f id)

-- Example on Stream

-- boilerplate
smap :: (a -> b) -> Stream a -> Stream b
smap f = transform (mapStream f) where
  mapStream :: (a -> b) -> StreamF a v -> StreamF b v
  mapStream f (Cons x xs) = Cons (f x) xs

instance BiFunctor StreamF where
  bimap f g (Cons x y) = Cons (f x) (g y)

smap' :: (a -> b) -> Stream a -> Stream b
smap' = gmap

--------------------------------------
-- Quasi-monadic operations

pjoin :: Functor f => Rec f (Rec f a) -> Rec f a
pjoin (Var x)  = x
pjoin (Mu g)   = Mu (map (fmap pjoin) . g . map Var)
pjoin (In r)   = In (fmap pjoin r)

unrollGraph :: Functor f => Graph f -> Graph f
unrollGraph g = Hide (pjoin (unroll (reveal g)))

unroll :: Functor f => Rec f (Rec f a) -> Rec f (Rec f a)
unroll (Mu g)  = In (head (g (repeat (pjoin (Mu g)))))
unroll (In r)  = In (fmap unroll r)

----------------------------------------------------
-- Traverse like operations

{-
hmapM
  :: (Monad m, Traversable f, Applicative m)
  => (forall a . m a -> a)
  -> (forall a . f a -> m (g a))
  -> Rec f a
  -> m (Rec g a)
hmapM runM f (Var x) = return (Var x)
hmapM runM f (Mu g)  = return (Mu (runM . sequence . map (join . liftM f . traverse (hmapM runM f)) . g))
hmapM runM f (In x)  = (join $ liftM f $ traverse (hmapM runM f) x) >>= return . In

hmapM2
  :: (Monad m, Traversable f, Applicative m)
  => (forall a . m a -> a)
  -> (f (Rec g a)
  -> m (g (Rec g a)))
  -> Rec f a
  -> m (Rec g a)
hmapM2 runM f (Var x) = return (Var x)
hmapM2 runM f (Mu g)  = return (Mu (runM . sequence . map (join . liftM f . traverse (hmapM2 runM f)) . g))
hmapM2 runM f (In x)  = (join $ liftM f $ traverse (hmapM2 runM f) x) >>= return . In
-}

-----------------------------------------------------
-- Ad-hoc Generic operations

-- A less general, but more pragmatic approach consist of using typeclasses
-- to divide the generic processing parts of a specific operation from the
-- structure specific parts of that operation.

-- We illustrate this technique on two operations: generic structural equality and generic
-- pretty printing.

----------------------
-- Equality

eqGraph :: EqF f => Graph f -> Graph f -> Bool
eqGraph g1 g2 = eqRec 0 (reveal g1) (reveal g2)

-- Notice, we are not using the gfold.
eqRec :: EqF f => Int -> Rec f Int -> Rec f Int -> Bool
eqRec _  (Var x)  (Var y)  = x == y
eqRec n  (Mu g)   (Mu h)   =
  let  a      = g (iterate succ n)
       b      = h (iterate succ n)
  in and $ zipWith (eqF (eqRec (n + length a))) a b
eqRec n  (In x)   (In y)   = eqF (eqRec n) x y
eqRec _  _        _        = False

-- Ad-hoc parametricity

class Functor f => EqF f where
  eqF :: (r -> r -> Bool) -> f r -> f r -> Bool

instance Eq a => EqF (StreamF a) where
   eqF eq (Cons x xs) (Cons y ys) = x == y && eq xs ys

instance Eq a => EqF (TreeF a) where
   eqF eq Empty Empty                      = True
   eqF eq (Fork x1 l1 r1) (Fork x2 l2 r2)  = x1 == x2 && eq l1 l2 && eq r1 r2
   eqF eq _ _                              = False

----------------------
-- Pretty Priting

showGraph :: ShowF f => Graph f -> String
showGraph g = showRec (iterate succ 'a') (reveal g)

showRec :: ShowF f => [Char] -> Rec f Char -> String
showRec _ (Var c)     = [c]
showRec seed (Mu f)   =
  let r            = f seed
      (fr, s')     = splitAt (length r) seed
  in "Mu (\n" ++ concat
     ["  "  ++ [a] ++ " => " ++ v ++ "\n" | (a,v) <-
            zip fr (map (showF (showRec s')) r)] ++ ")\n"
showRec seed (In fa)  = showF (showRec seed) fa

class Functor f => ShowF f where
  showF :: (r -> String) -> f r -> String

instance Show a => ShowF (StreamF a) where
  showF sh (Cons x xs) = show x ++ " : " ++ sh xs

instance Show a => ShowF (TreeF a) where
  showF sh Empty         = "Empty"
  showF sh (Fork x l r)  = "Fork " ++ show x ++
     "(" ++ sh l ++ ") (" ++ sh r ++ ")"
