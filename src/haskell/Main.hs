module Main where

import Data.HGit2.Blob
import Data.HGit2.Commit
import Data.HGit2.Config
import Data.HGit2.Errors
import Data.HGit2.Git2
import Data.HGit2.Index
import Data.HGit2.Tree
import Data.HGit2.Object
import Data.HGit2.Repository

main :: IO ()
main = do
  putStrLn "Opening repo in pwd"
  r <- openRepo "/Users/norm2782/src/hgit2/.git"
  putStrLn "Opened repo"
  case r of
    (Left err)   -> putStrLn $ "Error: " ++ show err
    (Right repo) -> do putStr "Chechking whether repo is empty... "
                       ie <- isEmpty repo
                       putStrLn $ show ie
                       putStr "Repository path... "
                       pt <- path repo GitRepoPath
                       putStrLn pt