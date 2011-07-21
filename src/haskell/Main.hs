module Main where

import Data.HGit2.Blob
import Data.HGit2.Commit
import Data.HGit2.Config
import Data.HGit2.Errors
import Data.HGit2.Git2
import Data.HGit2.Index
import Data.HGit2.Status
import Data.HGit2.Tree
import Data.HGit2.Object
import Data.HGit2.OID
import Data.HGit2.ODB
import Data.HGit2.ODBBackend
import Data.HGit2.Refs
import Data.HGit2.Repository
import Data.HGit2.Revwalk

main :: IO ()
main = do
  putStrLn "Opening repo in pwd"
  r <- openRepo "/Users/norm2782/src/hgit2/.git"
  putStrLn "Opened repo"
  case r of
    (Left err)   -> putStrLn $ "Error opening repo: " ++ show err
    (Right repo) -> do msg "Checking whether repo is empty... " (isEmpty repo)
                       msg "Repository path... " (path repo GitRepoPath)


msg str rhs = putStrLn . (str ++) . show =<< rhs
