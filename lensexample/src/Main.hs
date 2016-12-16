module Main where
import Control.Lens

-- https://github.com/ekmett/lens#lens-lenses-folds-and-traversals

-- In ghci you can do this:
-- ghci> :cmd reload_and_run_main
-- to reload and run main in one go
reload_and_run_main :: IO String
reload_and_run_main = return ":r\nmain\n"

main :: IO ()
main = do
  let x = (3, "dog")
  putStrLn $ show $ x ^. _1
  putStrLn . show $ view _1 x
  -- forall x y.  x ^. y == view y x

  -- ways to get chicken:
  let nest = (("dog", "chicken"), "elephant")
  putStrLn . show . snd $ fst nest
  putStrLn . show . view _2 $ view _1 nest
  putStrLn . show . (snd . fst) $ nest
  putStrLn . show . view (_1 . _2) $ nest  -- order is counterintuitive because continuation-passing style
  putStrLn . show $ nest ^.  _1 . _2  -- ed wanted to syntactically resemble object oriented dot
  
  -- making a modified version of nest is somewhat icky:
  let ((c, d), b) = nest
  let modified = ((c, "old-school"), b)
  putStrLn $ show modified
  
  let nest2 = (("dog", 4::Int), "elephant")
  let modified2 = set (_1 . _2) "this is not the same type" nest2
  putStrLn $ show modified2

  let mod3 = nest2 & _1 . _2 .~ "I was modded" -- practically oo dot notation and assignment
  putStrLn $ show mod3
  let myeither = Right (1::Int) :: Either String Int
  let myeither2 = Left "Major error message" :: Either String Int
  
  putStrLn . show $ (preview _Right myeither :: Maybe Int)

  putStrLn . show $ (preview _Right myeither2 :: Maybe Int)

  putStrLn . show $ (review _Right 1 :: Either String Int)   -- weird data constructor
  putStrLn . show $ (review _Left "an error" :: Either String Int) -- weird data constructor?
  putStrLn "DoneY"

