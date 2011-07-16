{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}

#include "git2.h"

module Data.HGit2.Commit where

import Data.Bits
import Data.HGit2.Git2
import Data.Maybe
import Foreign
import Foreign.C.String
import Foreign.C.Types
import System.IO.Unsafe

commitId :: Commit -> ObjID
commitId (Commit c) = unsafePerformIO $
  return . ObjID =<< {#call unsafe git_commit_id#} c

shortCommitMsg :: Commit -> String
shortCommitMsg (Commit c) = unsafePerformIO $
  peekCString =<< {#call unsafe git_commit_message_short#} c

commitMsg :: Commit -> String
commitMsg (Commit c) = unsafePerformIO $
  peekCString =<< {#call unsafe git_commit_message#} c

commitTime :: Commit -> TimeT
commitTime (Commit c) = unsafePerformIO $
  return =<< {#call unsafe git_commit_time#} c

timeOffset :: Commit -> Int
timeOffset (Commit c) = unsafePerformIO $
  return . fromIntegral =<< {#call unsafe git_commit_time_offset#} c

committer :: Commit -> Signature
committer (Commit c) = unsafePerformIO $
  return . Signature =<< {#call unsafe git_commit_committer#} c

author :: Commit -> Signature
author (Commit c) = unsafePerformIO $
  return . Signature =<< {#call unsafe git_commit_author#} c

tree :: Commit -> IO (Either GitError Tree)
tree (Commit c) = alloca $ \tree -> do
  res <- {#call git_commit_tree#} tree c
  retEither res $ fmap (Right . Tree) $ peek tree

treeOid :: Commit -> ObjID
treeOid (Commit c) = unsafePerformIO $
  return . ObjID =<< {#call unsafe git_commit_tree_oid#} c

parentCount :: Commit -> IO Int
parentCount (Commit c) =
  return . fromIntegral =<< {#call unsafe git_commit_parentcount#} c

parent :: Commit -> Int -> IO (Either GitError Commit)
parent (Commit c) n = alloca $ \parent -> do
  res <- {#call git_commit_parent#} parent c (fromIntegral n)
  retEither res $ fmap (Right . Commit) $ peek parent

parentObjID :: Commit -> Int -> IO (Maybe ObjID)
parentObjID (Commit c) n = do
  res <- {#call git_commit_parent_oid#} c (fromIntegral n)
  if res == nullPtr
    then return Nothing
    else return . Just . ObjID $ res

createCommit :: ObjID -> Repository -> Maybe String -> Signature -> Signature
             -> String -> Tree -> [Commit] -> IO (Maybe GitError)
createCommit (ObjID objId) (Repository r) mref (Signature ausig)
             (Signature comsig) msg (Tree t) ps = do
  updRef <- case mref of
              Nothing -> return nullPtr
              Just x  -> newCString x
  msgStr <- newCString msg
  carr   <- newArray [c | Commit c <- ps]
  res    <- {#call git_commit_create#} objId r updRef ausig comsig msgStr t cnt carr
  retMaybeRes res
  where cnt = fromIntegral $ length ps

