module Main where

import Data.HGit2.Blob
import Data.HGit2.Commit
import Data.HGit2.Config
import Data.HGit2.Common
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
import Data.HGit2.Tag
import Data.HGit2.Remote
import Data.HGit2.Transport
import System.Directory

pth :: String
pth = "/Users/norm2782/src/hgit2/test/.git"

-- TODO: Error when repo exists:
-- No repo found: GitEoverflow
-- Due to discover
main :: IO ()
main = do
  putStrLn $ "Looking for repo in " ++ pth
  disc <- discover pth False ""
  case disc of
    Left  err -> do putStrLn $ "Error or no repo found: " ++ show err
                    putStrLn $ "Initializing new repo in " ++ pth
                    inrp <- initRepo pth False
                    case inrp of
                      Left  err  -> putStrLn $ "Error initializing: " ++ show err
                      Right repo -> testRepo repo
    Right pth -> do repo <- openRepo pth
                    case repo of
                      Left  err -> putStrLn $ "Error opening repo: " ++ show err
                      Right repo -> testRepo repo

msg :: Show a => String -> IO a -> IO ()
msg str rhs = putStrLn . (str ++) . show =<< rhs

testRepo :: Repository -> IO ()
testRepo repo = do msg "Checking whether repo is empty... " (isEmpty repo)
                   msg "Repository path... " (path repo GitRepoPath)
                   putStrLn "Discovering again..."
                   disc <- discover pth False ""
                   case disc of
                     Left  err -> putStrLn $ "Discover error: "  ++ show err
                     Right pth -> putStrLn $ "Discover OK: " ++ pth
                   putStr "Removing repo... "
                   removeDirectoryRecursive pth
                   putStrLn "OK"
