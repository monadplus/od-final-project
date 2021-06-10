{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wincomplete-patterns#-}
module PropertyGraph where

--------------------------------------------

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Generic hiding (StreamF(..))
import Data.String
import qualified Data.List as List
import Data.Set (Set)
import qualified Data.Set as Set
import Fix
import qualified Data.Foldable as Foldable

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
  = VBool Bool
  | VInt Int
  | VDouble Double
  | VString String
  deriving stock (Eq, Ord)

instance Show Value where
  show  = \case
    VBool b -> show b
    VInt i -> show i
    VDouble d -> show d
    VString s -> show s

instance IsString Value where
  fromString = VString

type Properties = Map String Value

newtype Label = Label { unLabel :: String}
  deriving stock (Show)
  deriving newtype (Eq, Ord, IsString)

data Edge r = Edge
  { label :: Label
  , properties :: Properties
  , node :: r
  }
  deriving stock (Show)
  deriving stock (Functor, Foldable, Traversable)

data PropertyGraphF r = Node
  { label :: Label,
    properties :: Properties,
    edges :: [Edge r]
  }
  deriving (Functor, Foldable, Traversable)

type PropertyGraph = Graph PropertyGraphF

---------------------------------------------------------
-- Pretty Print

showProperties :: Properties -> String
showProperties props
  | null props = ""
  | otherwise =
      let xs = take 2 $ Map.toList props
      in "{" ++ showPairs xs ++ dots props ++ "}"
  where
    showPair (k, v) = show k ++ ":" ++ show v
    showPairs = List.intercalate "," . fmap showPair
    dots xs = if length xs > 2 then "..." else ""

showEdge :: (r -> String) -> Edge r -> String
showEdge sh Edge{label, properties, node} =
  "[" ++ unLabel label ++ showProperties properties ++ "]" ++ "->" ++ sh node

instance ShowF PropertyGraphF where
  showF sh Node {label, properties, edges} =
    nodeStr
      ++ List.intercalate ('\n' : ws ++ "-") ("" : fmap (showEdge sh) edges)
    where
      nodeStr =
        "("
          ++ unLabel label
          ++ showProperties properties
          ++ ")"
      ws = replicate (length  nodeStr) ' '

---------------------------------------------------------
-- Structural Equality

instance EqF Edge where
  eqF eq (Edge label1 properties1 r1) (Edge label2 properties2 r2) =
    label1 == label2
      && properties1 == properties2
      && eq r1 r2

-- >>> eqGraph pg1 pg1
-- True
--
-- >>> eqGraph pg1 pg2
-- False
instance EqF PropertyGraphF where
  eqF eq (Node label1 properties1 edges1) (Node label2 properties2 edges2) =
    label1 == label2
      && properties1 == properties2
      && and (zipWith (eqF eq) edges1 edges2)

---------------------------------------------------------
-- Algorithms

data V = V Label Properties
  deriving stock (Show, Eq, Ord)

type Reachable = Map V (Set V)

ppReachable :: Reachable -> String
ppReachable dict =
  List.intercalate "\n" $ fmap showPair (Map.toList dict)
  where
    showPair (v, vs) = show v ++ ":\n" ++ List.intercalate "\n" (fmap (('\t' :) . show) (Set.toList vs))

flatten :: Reachable -> Set V
flatten dict =
  let f (v, vs) = Set.insert v vs
  in foldMap f (Map.toList dict)

reachableF :: Rec PropertyGraphF Reachable -> Reachable
reachableF (Var x) = x
reachableF (Mu g)  =
  let g' = (map (f . fmap reachableF) . g)
  in Foldable.fold (fixVal (repeat Map.empty) g')
reachableF (In fa)  = f (fmap reachableF fa)

f :: PropertyGraphF Reachable -> Reachable
f (Node label props edges) =
  let adjacents = foldMap (\(Edge _ _ nodes) -> nodes) edges
      vertex = V label props
  in Map.singleton vertex (flatten adjacents)

reachableSet :: PropertyGraph -> Reachable
reachableSet = reachableF . reveal

testReachableSet :: PropertyGraph -> IO ()
testReachableSet = putStrLn . ppReachable . reachableSet

---------------------------------------------------------

pg1 =
  Hide
    ( Mu
      ( \(~(john : nancy : _)) ->
          [
            Node {
              label = "Person",
              properties = [("name", "John")],
              edges =
                [ Edge
                    { label = "fatherOf",
                      properties = [],
                      node = Var nancy
                    },
                  Edge
                    { label = "livingWith",
                      properties = [],
                      node = Var nancy
                    }
                ]
            },
            Node {
              label = "Robot",
              properties = [("name", "Nancy")],
              edges =
                [ Edge
                    { label = "daughterOf",
                      properties = [],
                      node = Var john
                    }
                ]
            }
          ]
      )
    )

-- John and Chris knows Nancy
-- Nancy knows Stuart
pg2 =
  Hide
    ( Mu
      ( \(~(john : chris : nancy : stuart: _)) ->
          [
            Node {
              label = "Person",
              properties = [("name", "John")],
              edges =
                [ Edge
                    { label = "Knows",
                      properties = [],
                      node = Var nancy
                    }
                ]
            },
            Node {
              label = "Person",
              properties = [("name", "Chris")],
              edges =
                [ Edge
                    { label = "Knows",
                      properties = [],
                      node = Var nancy
                    }
                ]
            },
            Node {
              label = "Person",
              properties = [("name", "Nancy")],
              edges =
                [ Edge
                    { label = "Knows",
                      properties = [],
                      node = Var stuart
                    }
                ]
            },
            Node {
              label = "Person",
              properties = [("name", "Stuart")],
              edges = []
            }
          ]
      )
    )

pg3 =
  Hide
    ( In
        ( Node
            { label = "Author",
              properties = [("name", "Nietzsche")],
              edges =
                [ Edge
                    { label = "AuthorOf",
                      properties = [],
                      node =
                        In
                          ( Node
                              { label = "Book",
                                properties = [("title", "Also sprach Zarathustra")],
                                edges =
                                  [ Edge
                                      { label = "AuthorOf",
                                        properties = [],
                                        node =
                                          In
                                            ( Node
                                                { label = "Book",
                                                  properties = [("title", "Also sprach Zarathustra")],
                                                  edges = []
                                                }
                                            )
                                      }
                                  ]
                              }
                          )
                    }
                ]
            }
        )
    )
