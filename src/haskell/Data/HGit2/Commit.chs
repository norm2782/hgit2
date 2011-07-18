{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}

#include <git2/commit.h>

module Data.HGit2.Commit where

import Data.HGit2.Git2
import Data.HGit2.Errors
import Data.HGit2.Repository
import Data.HGit2.Types
import Data.HGit2.Signature
import Data.HGit2.OID
import Data.HGit2.Tree
import Data.Maybe ()
import Foreign
import Foreign.C.String
import Foreign.C.Types

newtype Commit = Commit CPtr

instance CWrapper Commit where
  unwrap (Commit c) = c

oidCall :: (CPtr -> IO CPtr) -> Commit -> OID
oidCall = flipUSCall (return . OID)

strCall :: (CPtr -> IO CString) -> Commit -> String
strCall = flipUSCall peekCString

sigCall :: (CPtr -> IO CPtr) -> Commit -> Signature
sigCall = flipUSCall (return . Signature)

commitId :: Commit -> OID
commitId = oidCall {#call unsafe git_commit_id#}

treeOid :: Commit -> OID
treeOid = oidCall {#call unsafe git_commit_tree_oid#}

shortCommitMsg :: Commit -> String
shortCommitMsg = strCall {#call unsafe git_commit_message_short#}

commitMsg :: Commit -> String
commitMsg = strCall {#call unsafe git_commit_message#}

commitTime :: Commit -> TimeT
commitTime = usCall {#call unsafe git_commit_time#} (return =<<)

timeOffset :: Commit -> Int
timeOffset = usCall {#call unsafe git_commit_time_offset#} (return . fromIntegral =<<)

committer :: Commit -> Signature
committer = sigCall {#call unsafe git_commit_committer#}

author :: Commit -> Signature
author = sigCall {#call unsafe git_commit_author#}

tree :: Commit -> IO (Either GitError Tree)
tree (Commit c) = alloca $ \tr -> do
  res <- {#call git_commit_tree#} tr c
  retEither res $ fmap (Right . Tree) $ peek tr

parentCount :: Commit -> IO Int
parentCount = wrapToMNum {#call git_commit_parentcount#}

parent :: Commit -> Int -> IO (Either GitError Commit)
parent (Commit c) n = alloca $ \prnt -> do
  res <- {#call git_commit_parent#} prnt c (fromIntegral n)
  retEither res $ fmap (Right . Commit) $ peek prnt

parentOID :: Commit -> Int -> IO (Maybe OID)
parentOID (Commit c) n = do
  res <- {#call git_commit_parent_oid#} c (fromIntegral n)
  if res == nullPtr
    then return Nothing
    else return . Just . OID $ res

createCommit :: OID -> Repository -> Maybe String -> Signature -> Signature
             -> String -> Tree -> [Commit] -> IO (Maybe GitError)
createCommit (OID o) (Repository r) mref (Signature ausig)
             (Signature comsig) msg (Tree t) ps = do
  updRef <- case mref of
              Nothing -> return nullPtr
              Just x  -> newCString x
  msgStr <- newCString msg
  carr   <- newArray [c | Commit c <- ps]
  retMaybe =<< {#call git_commit_create#} o r updRef ausig comsig msgStr t
                                             (fromIntegral $ length ps) carr
