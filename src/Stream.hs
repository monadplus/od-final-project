{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables#-}
module Stream where

----------------------------------------------------------------

import Fix

----------------------------------------------------------------

-- Allow modelling back-edges with simple mu-binders.
-- Notice, only back-edges are needed since streams are linear.

data PStream a v
  = Var v
  | Mu (v -> PStream a v)
  | Cons a (PStream a v)
newtype Stream a = HideStream {revealStream :: forall v . PStream a v}

-- Inductive interpretion: finite cyclic streams.

-- Examples:
s1 = HideStream (Cons 1 (Mu (\v -> Cons 2 (Var v))))
s2 = HideStream (Mu (\v -> Cons 1 (Cons 2 (Var v))))

-- Acyclic (and infinite) streams such as the stream of natural numbers
-- are not representable under this interpretation.
--
-- Coinductive interpretation allow infinite streams but some operations are no longer valid.

-- ad-hoc
elems' :: Stream a -> [a]
elems' = pelems . revealStream where
  pelems :: PStream a [a] -> [a]
  pelems (Var v)      = v
  pelems (Mu g)       = pelems (g []) -- fix would generate an infinite list
  pelems (Cons x xs)  = x : pelems xs

foldStream :: (a -> b -> b) -> b -> Stream a -> b
foldStream f k = pfoldStream . revealStream where
  pfoldStream (Var x)       = x
  pfoldStream (Mu g)        = pfoldStream (g k)
  pfoldStream (Cons x xs)   = f x (pfoldStream xs)

-- >>> elems s1
-- [1,2]
elems :: Stream a -> [a]
elems = foldStream (:) []

-------------------------------------------------
-- Cyclic Folds on Stream

cfoldStream :: (a -> b -> b) -> Stream a -> b
cfoldStream f = pcfoldStream . revealStream where
  pcfoldStream (Var x)      = x
  pcfoldStream (Mu g)       = fix (pcfoldStream . g)
  pcfoldStream (Cons x xs)  = f x (pcfoldStream xs)

{- Unravelling cfoldStream s2

fix f where
  = f (fix f)
  = (\v -> pcfoldStream (Cons 2 (Var v))) (fix f)
  = pcfoldStream $ Cons 2 (Var $ fix f)
  = pcfoldStream $ Cons 2 (Var $ f (fix f))
  = pcfoldStream $ Cons 2 (Var ( \v -> pcfoldStream $ Cons 2 (Var v) (fix f) ))
  = pcfoldStream $ Cons 2 (Var (pcfoldStream $ Cons 2 (Var $ fix f))
  ...
-}

-- >>> toList s2
-- [1,2,1,2,...]
toList :: Stream a -> [a]
toList  = cfoldStream (:)

---------------------
-- Pretty Print

-- ad-hoc
pp :: Show a => Stream a -> String
pp = ppp . revealStream where
   ppp (Var v)      = v
   ppp (Mu g)       = fix (ppp . g)
   ppp (Cons x xs)  = show x ++ " : " ++ ppp xs

instance Show a => Show (Stream a) where
  show = pp'

pp' :: Show a => Stream a -> String
pp' = cfoldStream (\x s -> show x ++ " : " ++ s)

--------------------------------------------------
-- Sharing-preserving Transformations

instance Functor Stream where
  fmap = smap

smap :: (a -> b) -> Stream a -> Stream b
smap f s = HideStream (psmap f (revealStream s)) where
  psmap f (Var v)      = Var v
  psmap f (Mu g)       = Mu (psmap f . g)
  psmap f (Cons x xs)  = Cons (f x) (psmap f xs)

--------------------------------------------------
-- Structural equality

instance Eq a => Eq (Stream a) where
  s1 == s2 = peq 0 (revealStream s1) (revealStream s2)

-- No cycles.
peq ::
  Eq a =>
  Int ->
  PStream a Int ->
  PStream a Int ->
  Bool
peq n (Var x) (Var y) = x == y
peq n (Mu f) (Mu g) = peq (n + 1) (f n) (g n)
peq n (Cons x xs) (Cons y ys) = x == y && peq n xs ys
peq _ _ _ = False

--------------------------------------------------
-- Quasi-monad Structure

-- return
retPStream :: v -> PStream a v
retPStream = Var

-- join
joinPStream :: PStream a (PStream a v) -> PStream a v
joinPStream (Var v)      = v
joinPStream (Mu g)       = Mu (joinPStream . g . Var)
joinPStream (Cons x xs)  = Cons x (joinPStream xs)

-- join is useful to define various operations on stream:

-- unrolls a cycle once
unrollStream :: Stream a -> Stream a
unrollStream s = HideStream (joinPStream (punroll (revealStream s)))

punroll
  :: PStream a (PStream a v)
  -> PStream a (PStream a v)
punroll (Mu g)       = g (joinPStream (Mu g))
punroll (Cons x xs)  = Cons x (punroll xs)

substPStream :: PStream a v -> (v -> PStream a v) -> PStream a v
substPStream (Var v)      f  = f v
substPStream (Mu g)       f  = Mu (\x -> substPStream (g x) f)
substPStream (Cons x xs)  f  = Cons x (substPStream xs f)

expandStream :: Stream a -> Stream a
expandStream s = HideStream (unfoldPStream (revealStream s)) where
  unfoldPStream (Mu g)       = Mu (\x -> substPStream (g x) (const (g x)))
  unfoldPStream (Cons x xs)  = Cons x (unfoldPStream xs)

--------------------------------------------------
-- Hard operations

-- Some operations like 'stail' are not so easy to encode using the combinators defined.

-- >>> s2 = HideStream (Mu (\v -> Cons 1 (Cons 2 (Var v))))
-- >>> stail s2
-- Mu (\lambda a -> 2 : 1 : a)
stail :: Stream a -> Stream a
stail s = HideStream (joinPStream (ptail (revealStream s))) where
  ptail (Cons x xs)  = xs
  ptail (Mu g)       = Mu (\x ->
    let  phead (Mu g)       = phead (g x)
         phead (Cons y ys)  = y
    in ptail (g (Cons (phead (g x)) x)))
--  ^^^ Adds the whole recursive binder except its head to the start of the binder.
