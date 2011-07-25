{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}

#include <git2/status.h>

module Data.HGit2.Status where

import Data.HGit2.Git2
import Data.HGit2.Repository
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

foreachStatus :: (Num b, Enum a) => Repository -> FunPtr (Ptr CChar -> CUInt
              -> Ptr () -> IO CInt) -> Ptr () -> IO (Either a b)
foreachStatus (Repository fp) f p =
  withForeignPtr fp $ \r -> do
    res <- {#call git_status_foreach#} r f p
    return $ if res == 0
               then Left  $ toEnum . fromIntegral $ res
               else Right $ fromIntegral res

-- | Get file status for a single file
fileStatus :: Repository -> String -> IO Int
fileStatus (Repository fp) fl =
  withForeignPtr fp $ \r ->
    withCString fl $ \loc -> alloca $ \out -> do
    _ <- {#call git_status_file#} out r loc
    return . fromIntegral =<< peek out
