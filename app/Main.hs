module Main where

import Generic
import Grammar

main :: IO ()
main = do
  putStr $ showGraph g1
  print $ nullable g1
  print $ firstSet g1
  print $ firstSet g2
  putStr $ showGraph (normalize2 g1)
  putStr $ showGraph (atrans g1)
