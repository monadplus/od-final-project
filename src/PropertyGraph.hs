{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wincomplete-patterns#-}
module PropertyGraph where

--------------------------------------------

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Generic hiding (StreamF(..))

--------------------------------------------

{-
Property graph:

Nodes = entities
Edges = relations between entities
Nodes and edges may be labeled and may have a set of properties (key-value pairs)
Edges are directed
Multi-graphs are allowed (more than one edge per node).

Graph operations:
- Content-based queries
- Topological queries: adjacency, reachability, label-constrained reachability, pattern matching (graph isomorphism problem)
- Hybrid approaches

Graph Metrics:
- Centrility: page rank, betweenness, closeness
- Path finding algorithms: minimum weight spanning tree, single source shortest path
- Community detection algorithms: triangle counting, louvain, strongly connected components
-}

data Value
  = VString String
  | VInt Int

data PropertyGraphF r =
    Node { node :: (String, Map String Value)
         , edges :: [r]
         }
  deriving (Functor, Foldable, Traversable)

type PropertyGraph = Graph PropertyGraphF

-- (author:Author {name: "Arnau"})
pg1 = Hide (In (Node {node = ("Author", Map.empty), edges = []}))

-- (a: Author {name: "Arnau"}) - [r: AuthorOf] - (b: Book {title: "Final Project"})
pg2 = Hide (In (Node {node = ("Author", Map.empty),
                      edges = []}))
