{-# LANGUAGE RankNTypes #-}

-- in the REPL do ghci> :set -XRankNTypes instead
module Main where

type NaturalTransformation f g = forall a . f a -> g a



main :: IO ()
main = do
  putStrLn "hello world"
