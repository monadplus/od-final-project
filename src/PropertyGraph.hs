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
  deriving stock (Eq)
instance Show Value where
  show  = \case
    VString s -> show s
    VInt s -> show s

instance IsString Value where
  fromString = VString

type Properties = Map String Value

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

showEdge :: (r -> String) -> Edge r -> String
showEdge sh Edge{label, properties, node} =
  "[" ++ unLabel label ++ showProperties properties ++ "]" ++ "->" ++ sh node

data PropertyGraphF r = Node
  { label :: Label,
    properties :: Properties,
    edges :: [Edge r]
  }
  deriving (Functor, Foldable, Traversable)

type PropertyGraph = Graph PropertyGraphF

instance ShowF PropertyGraphF where
  showF sh Node {label, properties, edges} =
    "("
      ++ unLabel label
      ++ showProperties properties
      ++ ")"
      ++ if null edges then "" else "-"
      ++ List.intercalate "\n-" (fmap (showEdge sh) edges)

pg1 =
  Hide
    ( In
        ( Node
            { label = "Author",
              properties = [("name", "Arnau")],
              edges = []
            }
        )
    )

-- >>> putStrLn $ showGraph pg2
pg2 =
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
                                edges = []
                              }
                          )
                    }
                ]
            }
        )
    )

-- Mutually recursive edges
-- Multi-edge

pg3 =
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
              label = "Person",
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
