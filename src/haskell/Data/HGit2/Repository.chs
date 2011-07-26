{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}

#include <git2/repository.h>

module Data.HGit2.Repository where

import Data.HGit2.Git2
import Data.HGit2.Index
import Data.HGit2.ODB
import Data.HGit2.Config
import Foreign
import Foreign.C.String
import Foreign.C.Types


newtype Repository = Repository CPtr

instance CWrapper Repository where
  unwrap (Repository r) = r

{#enum git_repository_pathid as RepositoryPathID {underscoreToCase}#}

-- | Open a git repository.
--
-- The 'path' argument must point to an existing git repository folder, e.g.
--
--    /path/to/my_repo/.git/ (normal repository)
--        objects/
--        index
--        HEAD
--
--    /path/to/bare_repo/    (bare repository)
--        objects/
--        index
--        HEAD
--
-- The function will automatically detect if 'path' is a normal or bare
-- repository or fail is 'path' is neither.
openRepo :: String -> IOEitherErr Repository
openRepo pth =
  withCString pth $ \pstr ->
  callPeek Repository (\out -> {#call git_repository_open#} out pstr)

-- | Open a git repository by manually specifying all its paths
openRepoObjDir :: String -> String -> String -> String
               -> IOEitherErr Repository
openRepoObjDir dir objDir idxFile workTree =
  withCString dir $ \dirStr ->
  withCString objDir $ \objDStr ->
  withCString idxFile $ \idxFStr ->
  withCString workTree $ \wtrStr ->
  callPeek Repository (\out -> {#call git_repository_open2#} out dirStr objDStr
                                                              idxFStr wtrStr)

-- | Open a git repository by manually specifying its paths and the object
-- database it will use.
openRepoODB :: String -> ODB -> String -> String -> IOEitherErr Repository
openRepoODB dir (ODB dfp) idxFile workTree =
  withForeignPtr dfp $ \db ->
  withCString dir $ \dirStr ->
  withCString idxFile $ \idxFStr ->
  withCString workTree $ \wtrStr ->
  callPeek Repository (\out -> {#call git_repository_open3#} out dirStr db
                                                              idxFStr wtrStr)

-- | Look for a git repository and copy its path in the given buffer.
-- The lookup start from base_path and walk across parent directories if
-- nothing has been found. The lookup ends when the first repository is found,
-- or when reaching a directory referenced in ceiling_dirs or when the
-- filesystem changes (in case across_fs is true).
--
-- The method will automatically detect if the repository is bare (if there is
-- a repository).
-- TODO: Size is not calculated correctly
discover :: String -> Bool -> String -> IOEitherErr String
discover startPath acrossFs ceilingDirs = alloca $ \out ->
  withCString startPath $ \spStr ->
  withCString ceilingDirs $ \cdsStr -> do
  res <- {#call git_repository_discover#} out (fromIntegral $ (length startPath * 2))
                                          spStr (fromBool acrossFs) cdsStr
  eitherPeekStr out id res

-- | Get the object database behind a Git repository
database :: Repository -> IO ODB
database = wrpToCstr ODB {#call git_repository_database#}

-- | Open the Index file of a Git repository
--
-- This returns a new and unique `git_index` object representing the active
-- index for the repository.
--
-- This method may be called more than once (e.g. on different threads).
--
-- Each returned `git_index` object is independent and suffers no race
-- conditions: synchronization is done at the FS level.
--
-- Each returned `git_index` object must be manually freed by the user, using
-- `git_index_free`.
index :: Repository -> IOEitherErr Index
index (Repository fp) =
  withForeignPtr fp $ \r ->
  callPeek Index (\out -> {#call git_repository_index#} out r)

-- | Creates a new Git repository in the given folder.
initRepo :: String -> Bool -> IOEitherErr Repository
initRepo pth bare =
  withCString pth $ \pstr ->
  callPeek Repository
            (\out -> {#call git_repository_init#} out pstr (fromBool bare))

-- | Check if a repository's HEAD is detached
--
-- A repository's HEAD is detached when it points directly to a commit instead
-- of a branch.
isHeadDetached :: Repository -> IO Bool
isHeadDetached = wrpToBool {#call git_repository_head_detached#}

-- | Check if the current branch is an orphan
--
-- An orphan branch is one named from HEAD but which doesn't exist in the refs
-- namespace, because it doesn't have any commit to point to.
isHeadOrphan :: Repository -> IO Bool
isHeadOrphan = wrpToBool {#call git_repository_head_orphan#}

-- | Check if a repository is empty
--
-- An empty repository has just been initialized and contains no commits.
isEmpty :: Repository -> IO Bool
isEmpty = wrpToBool {#call git_repository_is_empty#}

-- | Get one of the paths to the repository
--
-- Possible values for `id`:
--
--  GIT_REPO_PATH: return the path to the repository
--  GIT_REPO_PATH_INDEX: return the path to the index
--  GIT_REPO_PATH_ODB: return the path to the ODB
--  GIT_REPO_PATH_WORKDIR: return the path to the working directory
path :: Repository -> RepositoryPathID -> IO String
path (Repository fp) pid =
  withForeignPtr fp $ \r ->
  peekCString =<< {#call git_repository_path#} r (unEnum pid)

-- | Check if a repository is bare
isBare :: Repository -> IO Bool
isBare = wrpToBool {#call git_repository_is_bare#}

-- | Retrieve the relevant configuration for a repository
--
-- By default he returned `git_config` instance contains a single configuration
-- file, the `.gitconfig' file that may be found inside the repository.
--
-- If the `user_config_path` variable is not NULL, the given config file will
-- be also included in the configuration set. On most UNIX systems, this file
-- may be found on `$HOME/.gitconfig`.
--
-- If the `system_config_path` variable is not NULL, the given config file will
-- be also included in the configuration set. On most UNIX systems, this file
-- may be found on `$PREFIX/etc/gitconfig`.
--
-- The resulting `git_config` instance will query the files in the following
-- order:
--
-- - Repository configuration file
-- - User configuration file
-- - System configuration file
--
-- The method will fail if any of the passed config files cannot be found or
-- accessed.
--
-- The returned `git_config` instance is owned by the caller and must be
-- manually free'd once it's no longer on use.
--
-- TODO: DEal with null strings like Maybe values
config :: Repository -> String -> String -> IOEitherErr Config
config (Repository rfp) up sp =
  withForeignPtr rfp $ \r ->
    withCString up $ \upt ->
    withCString sp $ \spt ->
    callPeek Config (\out -> {#call git_repository_config#} out r upt spt)

