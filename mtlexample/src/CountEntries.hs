
-- http://book.realworldhaskell.org/read/monad-transformers.html
module CountEntries (
  listDirectory,
  countEntriesTrad,
  demoCountEntries
  ) where

import Control.Monad.Trans (liftIO)
import Control.Monad.Writer (WriterT, tell, execWriterT)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class(lift)
import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Identity
import Control.Monad (forM, liftM, forM_, when)
import Data.Char (isAlpha, isNumber, isPunctuation)

import System.Directory (
  doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))

import System.Directory (doesDirectoryExist)
import System.FilePath ((</>))


listDirectory :: FilePath -> IO [String]
listDirectory =
  (liftM (filter notDots)) . getDirectoryContents
  where
    notDots p = p /= "." && p /= ".."

-- ghci> listDirectory "."
-- ghci> listDirectory "/Users/darrenrhea"
-- it does not like tilde


countEntriesTrad :: FilePath -> IO [(FilePath, Int)]
countEntriesTrad path = do
  contents <- listDirectory path
  rest <- forM contents $ \name -> do
            let newName = path </> name
            isDir <- doesDirectoryExist newName
            if isDir
              then countEntriesTrad newName
              else return []
  return $ (path, length contents) : concat rest


countEntries :: FilePath -> WriterT [(FilePath, Int)] IO ()
countEntries path = do
  contents <- liftIO . listDirectory $ path
  tell [(path, length contents)]
  forM_ contents $ \name -> do
    let newName = path </> name
    isDir <- liftIO . doesDirectoryExist $ newName
    when isDir $ countEntries newName


demoCountEntries :: IO ()
demoCountEntries = do
  temp <- countEntriesTrad  "fixtures"
  print $ temp
  temp <- execWriterT (countEntries  "fixtures")
  print $ temp
  putStrLn "Hello world"