{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}

#include <git2/status.h>

module Data.HGit2.Status where

import Data.HGit2.Git2
import Data.HGit2.Repository
import Data.HGit2.Errors
import Foreign
import Foreign.C.String
import Foreign.C.Types

{#enum define Status { GIT_STATUS_CURRENT as Current
                     , GIT_STATUS_INDEX_NEW as IndexNew
                     , GIT_STATUS_INDEX_MODIFIED as IndexModified
                     , GIT_STATUS_INDEX_DELETED as IndexDeleted
                     , GIT_STATUS_WT_NEW as WtNew
                     , GIT_STATUS_WT_MODIFIED as WtModified
                     , GIT_STATUS_WT_DELETED as WtDeleted
                     , GIT_STATUS_IGNORED as Ignored}#}

{-
/**
 * @param repo a repository object
 * @param callback the function to call on each file
 * @return GIT_SUCCESS or the return value of the callback which did not return 0;
 */
GIT_EXTERN(int) git_status_foreach(git_repository *repo, int (*callback)(const char *, unsigned int, void *), void *payload);
TODO: Review this
-}
-- | Gather file statuses and run a callback for each one.
--
-- The callback is passed the path of the file, the status and the data pointer
-- passed to this function. If the callback returns something other than
-- GitSuccess, this function will return that value.

foreachStatus :: Repository -> FunPtr (Ptr CChar -> CUInt -> CPtr -> IO CInt)
              -> CPtr -> IO (Either GitError Int)
foreachStatus (Repository r) f p = do
  res <- {#call git_status_foreach#} r f p
  return $ if res == 0
             then Left  $ toEnum . fromIntegral $ res
             else Right $ fromIntegral res
{-
/**
 * 
 *
 * @param status_flags the status value
 * @param repo a repository object
 * @param path the file to retrieve status for, rooted at the repo's workdir
 * @return GIT_SUCCESS
 */
GIT_EXTERN(int) git_status_file(unsigned int *status_flags, git_repository *repo, const char *path);

-}
-- | Get file status for a single file
-- TODO: Use data.bits for first argument
fileStatus :: Int -> Repository -> String -> IO GitError
fileStatus st (Repository r) fl = do
  fl' <- newCString fl
  res <- {#call git_status_file#} undefined undefined undefined-- TODO st r fl'
  return undefined
