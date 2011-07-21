{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}

#include <git2/index.h>

module Data.HGit2.Index where

import Data.Bits
import Data.HGit2.Git2
import Data.HGit2.Errors
import Data.Maybe()
import Foreign
import Foreign.C.String
import Foreign.C.Types

newtype Index      = Index CPtr
newtype IndexEntry = IndexEntry CPtr
newtype IndexEntryUnMerged = IndexEntryUnMerged CPtr

instance CWrapper Index where
  unwrap (Index i) = i

instance CWrapper IndexEntry where
  unwrap (IndexEntry ie) = ie

instance CWrapper IndexEntryUnMerged where
  unwrap (IndexEntryUnMerged ieu) = ieu

{#enum define IdxEntry { GIT_IDXENTRY_NAMEMASK as NameMask
                       , GIT_IDXENTRY_STAGEMASK as StageMask
                       , GIT_IDXENTRY_EXTENDED as ExtendedOrSkipWorkTree
                       , GIT_IDXENTRY_VALID as ValidOrExtended2
                       , GIT_IDXENTRY_STAGESHIFT as StageShift
                       , GIT_IDXENTRY_UPDATE as Update
                       , GIT_IDXENTRY_REMOVE as Remove
                       , GIT_IDXENTRY_UPTODATE as UpToDate
                       , GIT_IDXENTRY_ADDED as Added
                       , GIT_IDXENTRY_HASHED as Hashed
                       , GIT_IDXENTRY_UNHASHED as UnHashed
                       , GIT_IDXENTRY_WT_REMOVE as WTRemove
                       , GIT_IDXENTRY_CONFLICTED as Conflicted
                       , GIT_IDXENTRY_UNPACKED as Unpacked
                       , GIT_IDXENTRY_NEW_SKIP_WORKTREE as NewSkipWorkTree
                       , GIT_IDXENTRY_INTENT_TO_ADD as IntentToAdd
                       , GIT_IDXENTRY_SKIP_WORKTREE as SkipWorkTree
                       , GIT_IDXENTRY_EXTENDED2 as Extended2
                       }#}

-- TODO: Can we get this into IdxEntry somehow?
idxExtFlags :: Int
idxExtFlags = fromEnum IntentToAdd .|. fromEnum SkipWorkTree

-- | Create a new bare Git index object as a memory representation of the Git
-- index file in the provided path, without a repository to back it.
openIndex :: String -> IOEitherErr Index
openIndex path = callPeek Index
  (\out -> {#call git_index_open#} out =<< newCString path)

-- | Clear the contents (all the entries) of an index object. This clears the
-- index object in memory; changes must be manually written to disk for them to
-- take effect.
clearIndex :: Index -> IO ()
clearIndex = {#call git_index_clear#} . unwrap

-- | Free an existing index object.
freeIndex :: Index -> IO ()
freeIndex = {#call git_index_free#} . unwrap

-- | Update the contents of an existing index object in memory by reading from
-- the hard disk.
readIndex :: Index -> IOCanFail
readIndex = callRetMaybe {#call git_index_read#}

-- | Write an existing index object from memory back to disk using an atomic
-- file lock.
writeIndex :: Index -> IOCanFail
writeIndex = callRetMaybe {#call git_index_write#}

-- | Find the first index of any entries which point to given path in the Git
-- index.
findIndex :: Index -> String -> IO (Maybe Int)
findIndex (Index idx) path = do
  res <- {#call git_index_find#} idx =<< newCString path
  return $ if res >= 0
             then Just $ fromIntegral res
             else Nothing

-- | Remove all entries with equal path except last added
uniqIndex :: Index -> IO ()
uniqIndex = {#call git_index_uniq#} . unwrap

-- | Add or update an index entry from a file in disk
addIndex :: Index -> String -> Int -> IO (Maybe GitError)
addIndex (Index idx) path stage = do
  pth <- newCString path
  retMaybe =<< {#call git_index_add#} idx pth (fromIntegral stage)

-- | Add or update an index entry from an in-memory struct
addIndex2 :: Index -> IndexEntry -> IO (Maybe GitError)
addIndex2 (Index idx) (IndexEntry ie) =
  retMaybe =<< {#call git_index_add2#} idx ie

-- | Add (append) an index entry from a file in disk
appendIndex :: Index -> String -> Int -> IO (Maybe GitError)
appendIndex (Index idx) path stage = do
  pth <- newCString path
  retMaybe =<< {#call git_index_append#} idx pth (fromIntegral stage)

-- | Add (append) an index entry from an in-memory struct
appendIndex2 :: Index -> IndexEntry -> IO (Maybe GitError)
appendIndex2 (Index idx) (IndexEntry ie) =
  retMaybe =<< {#call git_index_append2#} idx ie

-- | Remove an entry from the index
remove :: Index -> Int -> IO (Maybe GitError)
remove (Index idx) n =
  retMaybe =<< {#call git_index_remove#} idx (fromIntegral n)

-- | Get a pointer to one of the entries in the index
getIndex :: Index -> Int -> IO (Maybe IndexEntry)
getIndex (Index idx) n =
  retRes IndexEntry =<< {#call git_index_get#} idx (fromIntegral n)

-- | Get the count of entries currently in the index
entryCount :: Index -> IO Int
entryCount = callRetNum {#call git_index_entrycount#}

-- | Get the count of unmerged entries currently in the index
entryCountUnMerged :: Index -> IO Int
entryCountUnMerged = callRetNum {#call git_index_entrycount_unmerged#}

retIEU :: CPtr -> IO (Maybe IndexEntryUnMerged)
retIEU = retRes IndexEntryUnMerged

-- | Get an unmerged entry from the index.
unmergedByPath :: Index -> String -> IO (Maybe IndexEntryUnMerged)
unmergedByPath (Index idx) path =
  retIEU =<< {#call git_index_get_unmerged_bypath#} idx =<< newCString path

-- | Get an unmerged entry from the index.
unmergedByIndex :: Index -> Int -> IO (Maybe IndexEntryUnMerged)
unmergedByIndex (Index idx) n =
  retIEU =<< {#call git_index_get_unmerged_byindex#} idx (fromIntegral n)

-- | Return the stage number from a git index entry
entryStage :: IndexEntry -> IO Int
entryStage = callRetNum {#call git_index_entry_stage#}
