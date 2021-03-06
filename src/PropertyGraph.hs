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
import Generic hiding (StreamF(..), Stream(..), Tree(..), TreeF(..))
import Data.String
import qualified Data.List as List
import Data.Set (Set)
import qualified Data.Set as Set
import Fix
import qualified Data.Foldable as Foldable
import Data.Maybe
import Data.Monoid (First(..))
import Data.Coerce(coerce)

--------------------------------------------

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

data Tree a = Tree a [Tree a]
  deriving stock (Show, Eq, Ord)

type Forest a = [Tree a]

findTree :: Eq a => a -> Forest a -> Maybe (Tree a)
findTree v = getFirst . foldMap (coerce . findNode) where
  findNode tree@(Tree a descendents)
    | a == v = Just tree
    | otherwise = findTree v descendents

acyclic :: (Eq a, Ord a) => Tree a -> Tree a
acyclic = go Set.empty where
  go visited (Tree v descendents) =
    let visited' = Set.insert v visited
        filterRec =
          fmap (go visited') .
            List.filter (\(Tree v' _) -> Set.notMember v' visited')
    in Tree v (filterRec descendents)

flattenF :: PropertyGraphF (Forest V) -> Forest V
flattenF (Node label props edges) =
  let adjacents = foldMap (\(Edge _ _ nodes) -> nodes) edges
      vertex = V label props
      tree = acyclic (Tree vertex adjacents)
  in [tree]

flatten :: PropertyGraph -> Forest V
flatten = sfold' flattenF []

reachability :: V -> V -> PropertyGraph -> Bool
reachability orig dest = go . flatten  where
  go :: Forest V -> Bool
  go forest = fromMaybe False $ do
    origin <- findTree orig forest
    _ <- findTree dest [origin]
    return True

--------------------------------------------------------
-- Iterative algorithms

{-
Graph operations:
- Content-based queries
- Topological queries: adjacency, reachability, label-constrained reachability, pattern matching (graph isomorphism problem)
- Hybrid approaches

Graph Metrics:
- Centrility: page rank, betweenness, closeness
- Path finding algorithms: minimum weight spanning tree, single source shortest path
- Community detection algorithms: triangle counting, louvain, strongly connected components
-}

-- Modeling an iterative algorithm with convergence is hard.

fixValTol :: (Ord a, Num a) => a -> a -> (a -> a) -> a
fixValTol v tol f = if abs (v-v') < tol then v else fixVal v' f
    where v' = f v

--------------------------------------------------------
-- Printing for test

showName :: V -> String
showName (V _ props) = maybe "unnamed" show (Map.lookup "name" props)

ppTree :: Int -> Tree V -> String
ppTree width (Tree v descendents) =
  let prefix = showName v
  in prefix ++ ppTrees (width + length prefix) descendents

ppTrees :: Int -> [Tree V] -> String
ppTrees _ [] = ""
ppTrees width trees =
  let prefix = "--" :: String
      ws = List.replicate width ' '
  in List.intercalate ('\n' : ws ++ prefix) ("": fmap (ppTree (width + length prefix)) trees)

flattenAndPrint :: PropertyGraph -> IO ()
flattenAndPrint = putStrLn . ppTrees 0 . flatten

--------------------------------------------------------
-- Instances

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


scoreNode :: Double -> PropertyGraphF r
scoreNode score =
  Node
    { label = "Node",
      properties = [("score", VDouble score)],
      edges = []
    }

directEdge :: forall r. r -> Edge r
directEdge x =
  Edge
    { label = "",
      properties = [],
      node = x
    }

-- triangle topology (i.e. cyclic topology)
pg4 =
  Hide
    ( Mu
      ( \(~(n1 : n2 : n3 : _)) ->
          [
            (scoreNode 1.0){edges = [directEdge (Var n2)]},
            (scoreNode 1.0){edges = [directEdge (Var n3)]},
            (scoreNode 1.0){edges = [directEdge (Var n1)]}
          ]
      )
    )
