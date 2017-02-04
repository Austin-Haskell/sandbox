{-# LANGUAGE FlexibleContexts #-}

-- http://stackoverflow.com/questions/32579133/simplest-non-trivial-monad-transformer-example-for-dummies-iomaybe
module Main where
import Control.Exception
import Control.Monad.Loops(iterateWhile, untilJust)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class(lift)
import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Identity
import Control.Monad(guard)
import Data.Maybe(isJust, isNothing)
import Data.Char(isAlpha, isNumber, isPunctuation)
import Text.Read(readMaybe)
import CountEntries(
  listDirectory,
  countEntriesTrad,
  demoCountEntries)
-- gotcha: for an Either to be an instance of Show,
-- both types must be instances of Show

import qualified Control.Monad.Reader as X
import qualified Control.Monad.Except as X
import qualified Control.Monad.IO.Class as X


newtype MyError = MyError String deriving Show

-- see https://hackage.haskell.org/package/transformers-0.5.2.0/docs/Control-Monad-Trans-State-Lazy.html
-- State is a bit of a misnomer
-- the right name is ParticularStateEvolverFunction, check it:


possibilities1 :: [Int]
possibilities1 = [1,2,3,7]

possibilities2 :: [Int]
possibilities2 = [10,100,1000]

comb :: [Int]
comb = do
  let z = 90
  x <- possibilities1
  y <- possibilities2
  return (x + y + z)

{-
ghci> comb
[11,101,1001,12,102,1002,13,103,1003,17,107,1007]
-}

-- BEGIN section on implicit monads:

getthree :: (Monad m) => m Int
getthree = do
  return 3


prim :: (Monad m) => m Int
prim = do
  x <- getthree
  let y = 2
  let z = 2
  return (x + y + z)
{-

ghci> prim :: Maybe Int
Just 7

ghci> prim :: [Int]
[7]

ghci> prim :: Either String Int
Right 7

ghci> (prim :: ((->) String) Int) "dog"
7

ghci> runState (prim :: State [String] Int) ["cat", "dog"]
(7,["cat","dog"])
ghci>

-}
-- END section on implicit monads.

tick :: State Int Int
tick = do
  n <- get  -- get whatever the current state value is
  put (n+1)  -- mutate the state to be 1 more than than it was
  return (n * n)  -- emit the value n squared

ten :: State Int Int
ten = do
  n <- get  -- get whatever the current state value is
  put (10*n)  -- mutate it to ten times that
  return (n + 700)  -- emit the value 700


{-
ghci> execState (sequence [tick, ten, ten]) 3
400
-}

{-
ghci> execState (sequence [tick, tick, ten]) 3
50
-}

getCurrentState :: State Int Int
getCurrentState = do
  currentState <- get
  --put currentState
  return currentState

incrementByTheCurrentState :: Int -> State Int Int
incrementByTheCurrentState x = do
  currentState <- get
  -- put currentState  -- since the state does not change, this line is unnecessary
  return (x + currentState)

setMyStateTo :: Int -> State Int ()
setMyStateTo x = do
  put x
  return ()



decrementState :: State Int ()
decrementState = do
  x <- get
  put (x - 1)
  return ()


decrementStateIfPositive :: State Int ()
decrementStateIfPositive = do
  x <- get
  if x > 0 then
    put (x - 1)
  else
    return ()

gq :: Int -> State Int Int
gq x = do
  currentState <- get
  put currentState
  return (x + currentState)


appendComment :: String -> State [String] ()
appendComment str = do
  s <- get
  put (s ++ [str])


factorial :: Int -> State [String] Int
factorial n = do
  appendComment $ "Got in " ++ (show n)
  if n > 0 then do
    y <- factorial (n - 1)
    return (n * y)
  else
    return 1

-- the naive/unmemoized/bad recursive implementation of
-- the fibonacci sequence
fib :: Int -> State [String] Int
fib n = do
  appendComment $ "Calculating fib " ++ (show n)
  if n >= 2 then do
    x <- fib (n - 1)
    y <- fib (n - 2)
    return (x + y)
  else do
    appendComment "That's a base case, so 1"
    return 1

-- demo via:
-- ghci> runState (fib 6) []


thiing :: IO (Int, [String])
thiing = do
  return (runState (fib 6) [])


recursiveFunction :: Int -> [Int]
recursiveFunction s =
  if s > 0 then
    [s, s*s] ++ recursiveFunction (s - 1)
  else
    []


recursiveProcedure :: State Int [Int]
recursiveProcedure = do
  s <- getCurrentState
  if s > 0 then do
    decrementState
    x <- recursiveProcedure
    return ([s, s*s] ++ x)
  else
    return []

-- ghci> evalState recursiveProcedure 5
-- ghci> runState recursiveProcedure 5
-- ghci> execState recursiveProcedure 5

imperi :: Int -> State Int Int
imperi x = do
  decrementStateIfPositive
  a <- incrementByTheCurrentState x
  setMyStateTo (a*a)
  b <- incrementByTheCurrentState 10
  decrementState
  return b

descen :: Int -> State Int Int
descen x = do
  s <- get  -- get the current state
  if s > 0 then do  -- so long as it is positive
    put (s - 1)  -- decrement the state
    y <- descen (2*x)  -- call yourself with a twice bigger argument
    return y  -- return that
  else  -- in the case the state is zero
    return x  -- just return

{-
ghci> runState (descen 3) 7
(384,0)
-}

stateAtTheEnd :: Int -> Int
stateAtTheEnd s0 = execState (imperi s0) 7

valueAtTheEnd :: Int -> Int
valueAtTheEnd s0 = evalState (imperi s0) 7


dstateAtTheEnd :: Int -> Int
dstateAtTheEnd s0 = execState (descen s0) 7

dvalueAtTheEnd :: Int -> Int
dvalueAtTheEnd s0 = evalState (descen s0) 7

plusOne :: Int -> Int
plusOne n = execState tick n

plusT :: Int -> Int -> Int
plusT n x = execState (sequence $ replicate n tick) x



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


type Config = (Int,Int)

-- type App a = ExceptT MyError IO a
type App a = ReaderT Config (ExceptT MyError IO) a

-- ReaderT r m a  ~  r -> m a
-- ExceptT e m a  ~  m (Either e a)

-- ReaderT Config (ExceptT MyError IO) a
--      ~  Config -> (ExceptT MyError IO) a
--      ~  Config -> IO (Either MyError a)
--
--      ~  ReaderT (x :: Config -> (ExceptT MyError IO) a)
--      ~  ReaderT (x :: Config -> ExceptT (y :: IO (Either MyError a)))

-- ExceptT MyError (ReaderT Config IO) a
--      ~  (ReaderT Config IO) (Either MyError a)
--      ~  Config -> IO (Either MyError a)
--
--      ~ ExceptT (x :: (ReaderT Config IO) (Either MyError a))
--      ~ ExceptT (x :: ReaderT (y :: Config -> IO (Either MyError a)))


readLine = read <$> getLine

-- for logging, use Katip

{-
mainInteractive :: App String
mainInteractive = do
  a <- myvalueT
  --b <- ExceptT $ Right <$> read <$> getLine
  b <- lift readLine
  myfunctionT a b
-}

mainInteractive :: App String
mainInteractive = do
  a <- lift myvalueT
  (x,y) <- ask
  lift . lift $ print (x,y)
  --b <- ExceptT $ Right <$> read <$> getLine
  b <- lift . lift $ readLine
  lift $ myfunctionT a b


-- for reference: type App a = ReaderT Config (ExceptT MyError IO) a
-- myvalueT ::  ExceptT MyError IO Int
-- myfunctionT :: Int -> Int -> ExceptT MyError IO String

myvalueT' :: X.MonadError MyError m => m Int
myvalueT' = undefined

myfunctionT' :: X.MonadError MyError m => Int -> Int -> m String
myfunctionT' = undefined

print' :: (Show a,X.MonadIO m) => a -> m ()
print' = X.liftIO . print

mainInteractiveMtl
    :: (X.MonadError MyError m, X.MonadReader Config m, X.MonadIO m)
    => m String
mainInteractiveMtl = do
  a <- myvalueT'
  (x,y) <- X.ask
  print' (x,y)
  b <- X.liftIO readLine
  myfunctionT' a b



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

main10 :: IO ()
main10 = do
  w738 <- runExceptT (runReaderT mainInteractive (10,20))
  print w738


-- The validation test could be anything we want it to be.
isValid :: String -> Bool
isValid s = any isNumber s


getPassphrase :: MaybeT IO String
getPassphrase = do
  s <- lift getLine
  guard (isValid s) -- Alternative provides guard.
  return s


askPassphrase :: MaybeT IO String
askPassphrase = do
  lift $ putStrLn "Insert a passphrase that contains at least one number:"
  value <- getPassphrase  -- if this fails, it get no further and returns Nothing
  lift $ putStrLn ("Storing " ++ value ++ " in database...")
  return value  -- returns Just value


mainask :: IO ()
mainask = do
  rtn <- runMaybeT askPassphrase
  case rtn of
    Just x ->
      print "Success"
    Nothing ->
      print "Did not successfully set password"
  print "Duzun."
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

-- somewhat re-implementation of iterateWhile from Control.Monad.Loops
doowhile :: (a -> Bool) -> IO a -> IO a
doowhile predicate action = do
  outcome <- action
  if predicate outcome then
    doowhile predicate action
  else return outcome


getMaybeInt :: IO (Maybe Int)
getMaybeInt = do
  putStrLn "enter an Int:"
  str <- getLine
  return $ readMaybe str


insistInt :: IO Int
insistInt = do
  jst <- doowhile (isNothing) getMaybeInt
  case jst of
    Just x -> return x
    Nothing -> fail "This should never happen"

insistInt2 :: IO Int
insistInt2 = do
  jst <- iterateWhile (isNothing) getMaybeInt
  case jst of
    Just x -> return x
    Nothing -> fail "This should never happen"


insistInt3 :: IO Int
insistInt3 = untilJust getMaybeInt

data ElemAtError
  = IndexTooLarge
  | NegativeIndex


instance Show ElemAtError where
  show IndexTooLarge = "elemAt: index too large"
  show NegativeIndex = "elemAt: negative index"

instance Exception ElemAtError

-- https://www.stackbuilders.com/news/errors-and-exceptions-in-haskell
exceptionElemAt :: [a] -> Int -> a
exceptionElemAt _      n | n < 0 = throw NegativeIndex
exceptionElemAt []     _         = throw IndexTooLarge
exceptionElemAt (x:_)  0         = x
exceptionElemAt (_:xs) n         = exceptionElemAt xs (n - 1)


tryexample :: IO ()
tryexample = do
  -- either <- try getInt :: IO (Either SomeException Int)
  either <- try (
    do
      putStrLn "enter a nonzero number:"
      str <- getLine
      -- throw IndexTooLarge
      return (read str)) :: IO (Either SomeException Int)
  -- either <- try (evaluate (exceptionElemAt [3,4,5] 4)) :: IO (Either SomeException Int)
  case either of
    Left e -> print "Error tom"
    Right x -> print (x + 2)
  return ()
