{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}

#include <git2.h>

module Data.HGit2.Index where

import Data.Bits
import Data.HGit2.Git2
import Data.Maybe
import Foreign
import Foreign.C.String
import Foreign.C.Types

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
openIndex :: String -> IO (Either GitError Index)
openIndex path = alloca $ \index -> do
  pth <- newCString path
  res <- {#call git_index_open#} index pth
  retEither res $ fmap (Right . Index) $ peek index

-- | Clear the contents (all the entries) of an index object. This clears the
-- index object in memory; changes must be manually written to disk for them to
-- take effect.
clearIndex :: Index -> IO ()
clearIndex (Index idx) = {#call git_index_clear#} idx

-- | Free an existing index object.
freeIndex :: Index -> IO ()
freeIndex (Index idx) = {#call git_index_free#} idx

-- | Update the contents of an existing index object in memory by reading from
-- the hard disk.
readIndex :: Index -> IO (Maybe GitError)
readIndex (Index idx) = do
  res <- {#call git_index_read#} idx
  retMaybeRes res

-- | Write an existing index object from memory back to disk using an atomic
-- file lock.
writeIndex :: Index -> IO (Maybe GitError)
writeIndex (Index idx) = do
  res <- {#call git_index_write#} idx
  retMaybeRes res

-- | Find the first index of any entries which point to given path in the Git
-- index.
findIndex :: Index -> String -> IO (Maybe Int)
findIndex (Index idx) path = do
  pth <- newCString path
  res <- {#call git_index_find#} idx pth
  if res >= 0
    then return . Just $ fromIntegral res
    else return Nothing

-- | Remove all entries with equal path except last added
uniqIndex :: Index -> IO ()
uniqIndex (Index idx) = {#call git_index_uniq#} idx

-- | Add or update an index entry from a file in disk
addIndex :: Index -> String -> Int -> IO (Maybe GitError)
addIndex (Index idx) path stage = do
  pth <- newCString path
  res <- {#call git_index_add#} idx pth (fromIntegral stage)
  retMaybeRes res

-- | Add or update an index entry from an in-memory struct
addIndex2 :: Index -> IndexEntry -> IO (Maybe GitError)
addIndex2 (Index idx) (IndexEntry ie) = do
  res <- {#call git_index_add2#} idx ie
  retMaybeRes res

-- | Add (append) an index entry from a file in disk
appendIndex :: Index -> String -> Int -> IO (Maybe GitError)
appendIndex (Index idx) path stage = do
  pth <- newCString path
  res <- {#call git_index_append#} idx pth (fromIntegral stage)
  retMaybeRes res

-- | Add (append) an index entry from an in-memory struct
appendIndex2 :: Index -> IndexEntry -> IO (Maybe GitError)
appendIndex2 (Index idx) (IndexEntry ie) = do
  res <- {#call git_index_append2#} idx ie
  retMaybeRes res

-- | Remove an entry from the index
remove :: Index -> Int -> IO (Maybe GitError)
remove (Index idx) n = do
  res <- {#call git_index_remove#} idx (fromIntegral n)
  retMaybeRes res

-- | Get a pointer to one of the entries in the index
getIndex :: Index -> Int -> IO (Maybe IndexEntry)
getIndex (Index idx) n = do
  res <- {#call git_index_get#} idx (fromIntegral n)
  return $ if res == nullPtr
             then Nothing
             else Just $ IndexEntry res

-- | Get the count of entries currently in the index
entryCount :: Index -> IO Int
entryCount (Index idx) =
  return . fromIntegral =<< {#call git_index_entrycount#} idx

-- | Get the count of unmerged entries currently in the index
entryCountUnMerged :: Index -> IO Int
entryCountUnMerged (Index idx) =
  return . fromIntegral =<< {#call git_index_entrycount_unmerged#} idx

-- | Get an unmerged entry from the index.
unmergedByPath :: Index -> String -> IO (Maybe IndexEntryUnMerged)
unmergedByPath (Index idx) path = do
  pth <- newCString path
  res <- {#call git_index_get_unmerged_bypath#} idx pth
  return $ if res == nullPtr
             then Nothing
             else Just $ IndexEntryUnMerged res

-- | Get an unmerged entry from the index.
unmergedByIndex :: Index -> Int -> IO (Maybe IndexEntryUnMerged)
unmergedByIndex (Index idx) n = do
  res <- {#call git_index_get_unmerged_byindex#} idx (fromIntegral n)
  return $ if res == nullPtr
             then Nothing
             else Just $ IndexEntryUnMerged res

-- | Return the stage number from a git index entry
entryStage :: IndexEntry -> IO Int
entryStage (IndexEntry ie) =
  return . fromIntegral =<< {#call git_index_entry_stage#} ie
