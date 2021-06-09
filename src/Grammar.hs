{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wincomplete-patterns#-}
module Grammar where

--------------------------------------------------------

import Control.Applicative
import Data.Foldable hiding (concat, all, concatMap, foldl)
import Data.Traversable hiding (mapM, sequence)
import Control.Monad
import Data.List
import Control.Monad.State
import Control.Monad.Identity
import Generic

--------------------------------------------------------

-- Application of structured graphs: grammar analysis and transformations.
-- We discuss 3 different operations on grammars:
-- - nullability
-- - first set
-- - normalization

-- Grammar: mutually recursive collection of patterns, where
--          patterns can also refer to themselves or other patterns.

data PatternF a
  = Term String -- Terminal
  | Epsilon -- Empty string
  | Seq a a -- Intersection
  | Alt a a -- Union
  deriving (Functor, Foldable, Traversable)

-----------------------------------
-- Nullability

-- Nullability determines whether a given nonterminal
-- can produce the empty string.

nullF :: PatternF Bool -> Bool
nullF (Term s)     = False
nullF Epsilon      = True
nullF (Seq g1 g2)  = g1 && g2
nullF (Alt g1 g2)  = g1 || g2

-- >>> nullable g2
nullable :: Graph PatternF -> Bool
nullable = sfold nullF False

-- Does not terminate for some inputs
-- e.g. 'badNullable g2' will loop
badNullable :: Graph PatternF -> Bool
badNullable = cfold nullF

-- Problematic grammar:
-- left-recursive grammar a -> a | 'x'
g2 = Hide (Mu (
   \(~(a:_)) -> [Alt (Var a)  (In (Term "x"))]))

-- Definition of +
g3 =
  Hide
    ( Mu
        ( \(~(a : a' : _)) ->
            [ Alt (In (Term "a")) (Var a'),
              Alt (Var a) (In Epsilon)
            ]
        )
    )

-----------------------------------
-- First Set

-- The first set is the set of terminals that can start sentences
-- produced by a pattern.

-- Nullability and first set as inputs.
firstF :: PatternF (Bool, [String]) -> [String]
firstF (Term s)              = [s]
firstF Epsilon               = []
firstF (Seq (b1,a1) (_,a2))  = if b1 then union a1 a2 else a1
firstF (Alt (_,a1) (_,a2))   = union a1 a2

nullFirstF :: PatternF (Bool, [String]) -> (Bool, [String])
nullFirstF = compose (leftPart nullF) firstF

compose f g x = (f x, g x)

leftPart :: Functor f => (f a -> a) -> f (a,b) -> a
leftPart alg = alg . fmap fst

firstSet :: Graph PatternF -> [String]
firstSet = snd . sfold nullFirstF (False,[])

-----------------------------------
-- Normalization

{-
A grammar is normalized if each node as a simple structure,
where *only one* sequential/alternative composition
may appear on the right hand side of a rule.

For example, the normalized version of the grammar a -> 'x' a | 'y' a:

  a -> b | c
  b -> 'x' a
  c -> 'y' a

-}

-- General mechanism for creating a new graph by writing nodes one by one.
-- The new nodes are managed by the state monad.
-- State is the triplet (n,i,o) where
--   n: number of nodes
--   i: list of referenceable node identities
--   o: list of node definitions
type MGraph f a = State (Int, [a], [f (Rec f a)])

addNode x = do
  (pos, inn, out) <- get
  put (pos + 1, inn, out ++ [x])
  return $ Var (inn !! pos)

-- Copies leaf patterns (terminals and epsilons).
-- Creates new nodes for any composite patterns.
normF ::  PatternF (Rec PatternF a) -> MGraph PatternF a (Rec PatternF a)
normF x@(Term s)  = return $ In x
normF x@Epsilon   = return $ In x
normF x           = addNode x

-- traverses the actual graph
trans :: Rec PatternF a -> MGraph PatternF a (Rec PatternF a)
trans (Var x) = pure (Var x)
trans (Mu g)  = pure $ Mu (\l -> runIt (l, g l) (scan (g l)))
trans (In s)  = traverse trans s >>= normF
scan o        = traverse (traverse trans) o >>= addNodes

runIt (l, out) m = evalState m (length out,l,[])

addNodes new = do
   (_, _, nodes) <- get
   return (new ++ nodes)

normalize :: Graph PatternF -> Graph PatternF
normalize x = Hide (evalState (trans (reveal x)) (0,[],[]))

-- Notice, normalize is defined by pattern matching instead of using the generic transformations.
-- It does not fit well in with common recursion schemes.

instance ShowF PatternF where
  showF sh Epsilon = "$"
  showF sh (Term s)   = "`" ++ s ++ "'"
  showF sh (Seq a b) = sh a ++ " " ++ sh b
  showF sh (Alt a b) = sh a ++ " | " ++ sh b

-- >>> putStrLn $ showGraph g1
-- Mu (
--   a => b c | d
--   b => `a' b | $
--   c => `c' d | b
--   d => `b' a
-- )
--
-- >>> putStrLn $ showGraph (normalize g1)
-- Mu (
--   a => e | d
--   b => f | $
--   c => g | b
--   d => `b' a
--   e => b c
--   f => `a' b
--   g => `c' d
-- )
g1 = Hide (Mu (
   \(~(a:b:c:d:_)) ->
       [Alt (In (Seq (Var b) (Var c))) (Var d),               -- Rule 1
        Alt (In (Seq (In (Term "a")) (Var b))) (In Epsilon),  -- Rule 2
        Alt (In (Seq (In (Term "c")) (Var d))) (Var b),       -- Rule 3
        Seq (In (Term "b")) (Var a)                           -- Rule 4
       ]))

-----------------------------
-- Alternative formulation

transformM :: (Traversable t, Applicative m, Monad m)
     => (t (Rec f a) -> m (Rec f a))
     -> (([a], [t (Rec t a)]) -> m b -> [f (Rec f a)])
     -> ([t (Rec f a)] -> m b)
     -> Rec t a -> m (Rec f a)
transformM f m h = trans where
   trans (Var x)  = pure (Var x)
   trans (In s)   = traverse trans s >>= f
   trans (Mu g)   = pure $ Mu (\l -> m (l, g l) (scan (g l)))
   scan out       = traverse (traverse trans) out >>= h

normalize2 :: Graph PatternF -> Graph PatternF
normalize2 x = Hide (evalState (process x) (0,[],[])) where
   process = transformM normF runIt addNodes . reveal

-----------------------------------------

-- Transposes a grammmar (?)
atrans g = Hide (atrans' (reveal g))
atrans' = runIdentity .
    transformM (return . In . trans)
                (\_ -> runIdentity)
                (return . map trans)
  where trans (Term s) = Term s
        trans Epsilon = Epsilon
        trans (Alt a b) = Seq a b
        trans (Seq a b) = Alt a b
