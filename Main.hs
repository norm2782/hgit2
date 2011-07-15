module Main where

import Git2

main :: IO ()
main = do
  putStrLn "Opening repo in pwd"
  r <- openRepo "/Users/norm2782/src/libgit2/include/.git"
  putStrLn "Opened repo"
  case r of
    (Left err)   -> putStrLn $ "Error: " ++ show err
    (Right repo) -> do putStr "Chechking whether repo is empty... "
                       ie <- isEmpty repo
                       putStrLn $ show ie
                       putStr "Repository path... "
                       pt <- path repo GitRepoPath
                       putStrLn pt