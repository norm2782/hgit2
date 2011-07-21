{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}

#include <git2/revwalk.h>

module Data.HGit2.Revwalk where

import Data.HGit2.Git2
import Data.HGit2.Repository
import Data.HGit2.OID
import Foreign
import Foreign.C

{#enum define Sort { GIT_SORT_NONE as SortNone
                   , GIT_SORT_TOPOLOGICAL as SortTopo
                   , GIT_SORT_TIME as SortTime
                   , GIT_SORT_REVERSE as SortReverse  }#}

newtype RevWalk = RevWalk CPtr

instance CWrapper RevWalk where
  unwrap (RevWalk r) = r

-- | Allocate a new revision walker to iterate through a repo.
--
-- This revision walker uses a custom memory pool and an internal commit cache,
-- so it is relatively expensive to allocate.
--
-- For maximum performance, this revision walker should be reused for different
-- walks.
--
-- This revision walker is *not* thread safe: it may only be used to walk a
-- repository on a single thread; however, it is possible to have several
-- revision walkers in several different threads walking the same repository.
newWalk :: Repository -> IOEitherErr RevWalk
newWalk (Repository r) = alloca $ \out ->
  eitherPeek out RevWalk =<< {#call git_revwalk_new#} out r

-- | Reset the revision walker for reuse.
--
-- This will clear all the pushed and hidden commits, and leave the walker in a
-- blank state (just like at creation) ready to receive new commit pushes and
-- start a new walk.
--
-- The revision walk is automatically reset when a walk is over.
resetWalk :: RevWalk -> IO ()
resetWalk = {#call git_revwalk_reset#} . unwrap

-- | Mark a commit to start traversal from.
--
-- The given OID must belong to a commit on the walked repository.
--
-- The given commit will be used as one of the roots when starting the revision
-- walk. At least one commit must be pushed the repository before a walk can be
-- started.
pushWalk :: RevWalk -> OID -> IOCanFail
pushWalk (RevWalk r) (OID o) = retMaybe =<< {#call git_revwalk_push#} r o

-- | Mark a commit (and its ancestors) uninteresting for the output.
--
-- The given OID must belong to a commit on the walked repository.
--
-- The resolved commit and all its parents will be hidden from the output on
-- the revision walk.
hideWalk :: RevWalk -> OID -> IOCanFail
hideWalk (RevWalk r) (OID o) = retMaybe =<< {#call git_revwalk_hide#} r o

-- | Get the next commit from the revision walk.
--
-- The initial call to this method is *not* blocking when iterating through a
-- repo with a time-sorting mode.
--
-- Iterating with Topological or inverted modes makes the initial call blocking
-- to preprocess the commit list, but this block should be mostly unnoticeable
-- on most repositories (topological preprocessing times at 0.3s on the git.git
-- repo).
--
-- The revision walker is reset when the walk is over.
nextWalk :: RevWalk -> OID -> IOCanFail
nextWalk (RevWalk r) (OID o) = retMaybe =<< {#call git_revwalk_next#} o r

-- | Change the sorting mode when iterating through the repository's contents.
--
-- Changing the sorting mode resets the walker.
setSorting :: RevWalk -> Sort -> IO ()
setSorting (RevWalk r) s =
  {#call git_revwalk_sorting#} r (fromIntegral $ fromEnum s)

-- | Free a revision walker previously allocated.
freeWalk :: RevWalk -> IO ()
freeWalk = {#call git_revwalk_free#} . unwrap

-- | Return the repository on which this walker is operating.
walkerRepo :: RevWalk -> IO Repository
walkerRepo = callRetCons {#call git_revwalk_repository#} Repository
