module Main where

--import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except


newtype MyError = MyError String

data MyEither a b = Bad a | Good b deriving Show


instance Functor (MyEither errtype) where
  --fmap :: (a -> b) -> (MyEither errtype) a -> (MyEither errtype) b
  fmap _ (Bad x) = Bad x
  fmap h (Good x) = Good (h x)


instance Applicative (MyEither errtype) where
  pure x = Good x
  Good h <*> Good x = Good (h x)
  Bad msg <*> Good _ = Bad msg
  Good _ <*> Bad msg = Bad msg
  Bad _ <*> Bad msg = Bad msg


instance Monad (MyEither errtype) where
  Good x >>= f = f x
  Bad msg >>= _ = Bad msg


readingFile :: String -> IO String
readingFile name =
  if name == "crap.txt"
  then pure "doggie"
  else error "WTF"

myvalue ::  IO (Either MyError Int)
myvalue = pure $ pure 3

myvalueT ::  ExceptT MyError IO Int
myvalueT = pure 3
-- myvalueT = ExceptT myvalue
-- myvalueT = pure 3  -- this also works

badvalue ::  IO (Either MyError Int)
badvalue = pure $ Left (MyError "This is a mistake")

badvalueT ::  ExceptT MyError IO Int
badvalueT = throwE $ MyError "This is a mistake"


myfunction :: Int -> Int -> IO (Either MyError String)
myfunction x y = pure $ Right $ show $ (x + y)

myfunctionT :: Int -> Int -> ExceptT MyError IO String
myfunctionT x y = pure $ show $ (x + y)

-- this is what suck looks like:
main2 :: IO (Either MyError String)
main2 = do
  a <- myvalue
  b <- badvalue
  case a of
    Left e -> return $ Left e
    Right x -> case b of
      Left e -> return $ Left e
      Right y -> myfunction x y


mainT :: ExceptT MyError IO String
mainT = do
  a <- myvalueT
  b <- badvalueT
  myfunctionT a b



safeReadingFile :: String -> IO (Either MyError String)

safeReadingFile name =
  if name == "crap.txt"
  then pure (Right "doggie")
  else pure (Left $ MyError "WTF")


-- safeReadingFile2 :: String -> ExceptT MyError (IO String)
-- safeReadingFile2 x = undefined

co :: Either String Int
co = Right 73

g :: Int -> Either String Int
g x = if x == 0 then Left "ooops" else Right (100 `div` x)

main :: IO ()
main = do
  putStrLn "hello world"
  putStrLn . show $ g 3
  putStrLn . show $ (co >>= g)
  y <- (readingFile "crap.txt")
  putStrLn y
  --z <- (safeReadingFile "wrongname")
  --putStrLn z





{-
ghci> [\x -> x*2, \x -> x+7] <*> [3::Int, 4::Int]
[6,8,10,11]
ghci> pure 3 :: List Int

<interactive>:18:11: error:
    Not in scope: type constructor or class ‘List’
ghci> pure 3 :: [Int]
[3]
ghci> [1,2,3] >>= (\x -> [x, x*7, x+100])
[1,7,101,2,14,102,3,21,103]
-}

