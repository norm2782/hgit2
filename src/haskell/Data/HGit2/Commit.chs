{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}

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

-- | Get the id of a commit.
commitId :: Commit -> OID
commitId = unsafePerformIO . wrpToCstr OID {#call unsafe git_commit_id#}

-- | Get the id of the tree pointed to by a commit. This differs from `tree` in
-- that no attempts are made to fetch an object from the ODB.
treeOID :: Commit -> OID
treeOID = unsafePerformIO . wrpToCstr OID {#call unsafe git_commit_tree_oid#}

-- | Get the short (one line) message of a commit.
shortCommitMsg :: Commit -> String
shortCommitMsg = unsafePerformIO . wrpToStr
  {#call unsafe git_commit_message_short#}

-- | Get the full message of a commit.
commitMsg :: Commit -> String
commitMsg = unsafePerformIO . wrpToStr {#call unsafe git_commit_message#}

-- | Get the commit time (i.e. committer time) of a commit.
commitTime :: Commit -> TimeT
commitTime (Commit cfp) = unsafePerformIO $
  withForeignPtr cfp $ \p ->
  {#call unsafe git_commit_time#} p

-- | Get the commit timezone offset (i.e. committer's preferred timezone) of a
-- commit.
timeOffset :: Commit -> Int
timeOffset = unsafePerformIO . wrpToInt {#call unsafe git_commit_time_offset#}

-- | Get the committer of a commit.
committer :: Commit -> Signature
committer = unsafePerformIO . wrpToCstr Signature
  {#call unsafe git_commit_committer#}

-- | Get the author of a commit.
author :: Commit -> Signature
author = unsafePerformIO . wrpToCstr Signature
  {#call unsafe git_commit_author#}

-- | Get the tree pointed to by a commit.
tree :: Commit -> IOEitherErr Tree
tree (Commit cfp) =
  withForeignPtr cfp $ \c ->
  callPeek Tree (\out -> {#call git_commit_tree#} out c)

-- | Get the number of parents of this commit
parentCount :: Commit -> IO Int
parentCount = wrpToInt {#call git_commit_parentcount#}

-- | Get the specified parent of the commit.
parent :: Commit -> Int -> IOEitherErr Commit
parent (Commit cfp) n =
  withForeignPtr cfp $ \c ->
  callPeek Commit (\out -> {#call git_commit_parent#} out c (fromIntegral n))

-- | Get the oid of a specified parent for a commit. This is different from
-- `parent`, which will attempt to load the parent commit from the ODB.
parentOID :: Commit -> Int -> IO (Maybe OID)
parentOID (Commit cfp) n =
  withForeignPtr cfp $ \c -> do
  r <- mkFPtr =<< {#call git_commit_parent_oid#} c (fromIntegral n)
  retRes OID r

-- | Create a new commit in the repository
-- TODO: Support list of commits here
createCommit :: OID -> Repository -> Maybe String -> Signature -> Signature
             -> String -> Tree -> Commit -> IO (Maybe GitError)
createCommit (OID ofp) (Repository rfp) mref (Signature afp) (Signature cfp)
             msg (Tree tfp) (Commit mfp) =
  withForeignPtr ofp $ \o ->
  withForeignPtr rfp $ \r ->
  withForeignPtr afp $ \ausig ->
  withForeignPtr cfp $ \comsig ->
  withForeignPtr tfp $ \t ->
  withForeignPtr mfp $ \m ->
  withCString msg $ \msgStr -> do
  carr <- newArray [m]
  let cc' = cc carr o r ausig comsig t msgStr
  let ret = case mref of
              Nothing -> cc' nullPtr
              Just x  -> withCString x $ \str -> cc' str
  free carr
  ret
  where cc carr o r as cs t ms ref = retMaybe =<< {#call git_commit_create#} o
           r ref as cs ms t (fromIntegral (1 :: Int)) carr
