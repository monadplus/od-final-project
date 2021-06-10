{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
module Main where

----------------------------

import Generic
import Grammar
import Text.Printf
import PropertyGraph

----------------------------

main :: IO ()
main = do
  putStrLn "Graph g1:"
  putStr $ showGraph g1
  putStrLn $ printf "Is g1 nullable? %s" (show $ nullable g1)
  putStrLn $ printf "g1 first set: %s" (show $ firstSet g1)
  putStrLn "g1 normalized:"
  putStr $ showGraph (normalize g1)
  putStrLn "g1 atrans:"
  putStr $ showGraph (atrans g1)

  putStrLn $ printf "Is pg1 structurally equal to pg1? %s" (show $ eqGraph pg1 pg1)
  putStrLn $ printf "Is pg1 structurally equal to pg2? %s" (show $ eqGraph pg1 pg2)

  let john = V "Person" [("name", "John")]
      chris = V "Person" [("name", "Chris")]
      stuart = V "Person" [("name", "Stuart")]
  putStrLn $ printf "Is Stuart reachable from John? %s" (show $ reachability john stuart pg2)
  putStrLn $ printf "Is Chris reachable from John? %s" (show $ reachability john chris pg2)
