module Main where

----------------------------

import Generic
import Grammar
import Text.Printf

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
