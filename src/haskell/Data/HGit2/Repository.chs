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

repoIs :: (CPtr -> IO CInt) -> Repository -> IO Bool
repoIs ffi (Repository ptr) = return . toBool =<< ffi ptr

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
openRepo pth = do
  pstr <- newCString pth
  callPeek Repository (\out -> {#call git_repository_open#} out pstr)

-- | Open a git repository by manually specifying all its paths
openRepoObjDir :: String -> String -> String -> String -> IOEitherErr Repository
openRepoObjDir dir objDir idxFile workTree = do
  dirStr  <- newCString dir
  objDStr <- newCString objDir
  idxFStr <- newCString idxFile
  wtrStr  <- newCString workTree
  callPeek Repository (\out -> {#call git_repository_open2#} out dirStr objDStr idxFStr wtrStr)

-- | Open a git repository by manually specifying its paths and the object
-- database it will use.
openRepoODB :: String -> ODB -> String -> String -> IOEitherErr Repository
openRepoODB dir (ODB db) idxFile workTree = do
  dirStr  <- newCString dir
  idxFStr <- newCString idxFile
  wtrStr  <- newCString workTree
  callPeek Repository (\out -> {#call git_repository_open3#} out dirStr db idxFStr wtrStr)

-- | Look for a git repository and copy its path in the given buffer.
-- The lookup start from base_path and walk across parent directories if
-- nothing has been found. The lookup ends when the first repository is found,
-- or when reaching a directory referenced in ceiling_dirs or when the
-- filesystem changes (in case across_fs is true).
--
-- The method will automatically detect if the repository is bare (if there is
-- a repository).
discover :: CSize -> String -> Bool -> String -> IOEitherErr String
discover sz startPath acrossFs ceilingDirs = alloca $ \out -> do
  spStr  <- newCString startPath
  cdsStr <- newCString ceilingDirs
  res    <- {#call git_repository_discover#} out (fromIntegral sz) spStr
                                             (fromBool acrossFs) cdsStr
  eitherPeekStr out id res

-- | Get the object database behind a Git repository
database :: Repository -> IO ODB
database = callRetCons {#call git_repository_database#} ODB

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
index (Repository r) = callPeek Index
  (\out -> {#call git_repository_index#} out r)

-- | Free a previously allocated repository
--
-- Note that after a repository is free'd, all the objects it has spawned will
-- still exist until they are manually closed by the user with
-- `git_object_close`, but accessing any of the attributes of an object without
-- a backing repository will result in undefined behavior
free :: Repository -> IO ()
free = {#call git_repository_free#} . unwrap

-- | Creates a new Git repository in the given folder.
init :: String -> Bool -> IOEitherErr Repository
init pth bare = do
  pstr <- newCString pth
  callPeek Repository (\out -> {#call git_repository_init#} out pstr (fromBool bare))

-- | Check if a repository's HEAD is detached
--
-- A repository's HEAD is detached when it points directly to a commit instead
-- of a branch.
isHeadDetached :: Repository -> IO Bool
isHeadDetached = repoIs {#call git_repository_head_detached#}

-- | Check if the current branch is an orphan
--
-- An orphan branch is one named from HEAD but which doesn't exist in the refs
-- namespace, because it doesn't have any commit to point to.
isHeadOrphan :: Repository -> IO Bool
isHeadOrphan = repoIs {#call git_repository_head_orphan#}

-- | Check if a repository is empty
--
-- An empty repository has just been initialized and contains no commits.
isEmpty :: Repository -> IO Bool
isEmpty = repoIs {#call git_repository_is_empty#}

-- | Get one of the paths to the repository
--
-- Possible values for `id`:
--
--  GIT_REPO_PATH: return the path to the repository
--  GIT_REPO_PATH_INDEX: return the path to the index
--  GIT_REPO_PATH_ODB: return the path to the ODB
--  GIT_REPO_PATH_WORKDIR: return the path to the working directory
path :: Repository -> RepositoryPathID -> IO String
path (Repository r) pathID = peekCString =<< {#call git_repository_path#} r p
  where p = fromIntegral $ fromEnum pathID

-- | Check if a repository is bare
isBare :: Repository -> IO Bool
isBare = repoIs {#call git_repository_is_bare#}

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
config (Repository r) up sp = do
  upt <- newCString up
  spt <- newCString sp
  callPeek Config (\out -> {#call git_repository_config#} out r upt spt)

