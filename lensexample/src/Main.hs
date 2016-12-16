module Main where

import Control.Lens
-- https://github.com/ekmett/lens#lens-lenses-folds-and-traversals
main :: IO ()
main = do
  putStrLn "hello len r"
  let x = (3, "dog")
  putStrLn $ show $ x ^. _1
  putStrLn . show $ view _1 x
  -- forall x y.  x ^. y == view y x

  let nest = (("dog", "chicken"), "elephant")
  putStrLn . show . view _2 $ view _1 nest


