{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}

#include <git2/status.h>

module Data.HGit2.Status where

import Data.Bits
import Data.HGit2.Git2
import Data.Maybe
import Foreign
import Foreign.C.String
import Foreign.C.Types

{-
#define GIT_STATUS_CURRENT        0
/** Flags for index status */
#define GIT_STATUS_INDEX_NEW      (1 << 0)
#define GIT_STATUS_INDEX_MODIFIED (1 << 1)
#define GIT_STATUS_INDEX_DELETED  (1 << 2)

/** Flags for worktree status */
#define GIT_STATUS_WT_NEW         (1 << 3)
#define GIT_STATUS_WT_MODIFIED    (1 << 4)
#define GIT_STATUS_WT_DELETED     (1 << 5)

// TODO Ignored files not handled yet
#define GIT_STATUS_IGNORED        (1 << 6)



/**
 * Gather file statuses and run a callback for each one.
 *
 * The callback is passed the path of the file, the status and the data pointer
 * passed to this function. If the callback returns something other than
 * GIT_SUCCESS, this function will return that value.
 *
 * @param repo a repository object
 * @param callback the function to call on each file
 * @return GIT_SUCCESS or the return value of the callback which did not return 0;
 */
GIT_EXTERN(int) git_status_foreach(git_repository *repo, int (*callback)(const char *, unsigned int, void *), void *payload);

/**
 * Get file status for a single file
 *
 * @param status_flags the status value
 * @param repo a repository object
 * @param path the file to retrieve status for, rooted at the repo's workdir
 * @return GIT_SUCCESS
 */
GIT_EXTERN(int) git_status_file(unsigned int *status_flags, git_repository *repo, const char *path);

-}
