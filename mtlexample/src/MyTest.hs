-- http://stackoverflow.com/questions/32579133/simplest-non-trivial-monad-transformer-example-for-dummies-iomaybe

-- file-watch this via
-- $ stack build --file-watch mtlexample:exe:mytest

-- load this into the repl via
-- $ stack ghci mtlexample:exe:mytest

-- just run (does not recompile, although if you are file-watching soon it will be recompiled) it via:
-- stack exec mytest

module Main where

main :: IO ()
main = do
  putStrLn "Hello from MyTest7! "