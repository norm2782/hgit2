{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE CPP #-}

#include "git2.h"

module Git2 where

import Data.Primitive
import Foreign
import Foreign.C.Types

type OffT    = {#type git_off_t#}
type TimeT   = {#type git_time_t#}
type Ptr2Int = Ptr () -> IO CInt

newtype ObjDB      = ObjDB { unODB :: Ptr () }
newtype Repository = Repository { unRepository :: Ptr () }
newtype Index      = Index { unIndex :: Ptr () }

{#enum git_otype as OType {underscoreToCase}#}
{#enum git_rtype as RType {underscoreToCase}#}
{#enum git_repository_pathid as RepositoryPathID {underscoreToCase}#}
{#enum git_error as GitError {underscoreToCase}#}
{#enum git_odb_streammode as ODBStreamMode {underscoreToCase}#}

repoIs :: Ptr2Int -> Repository -> IO Bool
repoIs ffi (Repository ptr) = return . toBool =<< ffi ptr

openRepo :: String -> IO (Either GitError Repository)
openRepo path = undefined -- git_repository_open

openRepoObjDir :: String -> String -> String -> String -> IO (Either GitError Repository)
openRepoObjDir dir objDir idxFile workTree = undefined -- git_repository_open2

openRepoObjDb :: String -> ObjDB -> String -> String -> IO (Either GitError Repository)
openRepoObjDb dir db idxFile workTree = undefined -- git_repository_open3

-- TODO: size? GIT_EXTERN(int) git_repository_discover(char *repository_path, size_t size, const char *start_path, int across_fs, const char *ceiling_dirs);
discover :: String -> Bool -> String -> IO (Either GitError String)
discover startPath acrossFs ceilingDirs = undefined -- git_repository_discover

database :: Repository -> IO ObjDB
database (Repository r) = return . ObjDB =<< {#call git_repository_database#} r

index :: Repository -> IO (Either GitError Index)
index repo = undefined -- git_repository_index

free :: Repository -> IO ()
free (Repository r) = {#call git_repository_free#} r

init :: String -> Bool -> Either GitError Repository
init path isBare = undefined -- git_repository_init

isHeadDetached :: Repository -> IO Bool
isHeadDetached = repoIs {#call git_repository_head_detached#}

isHeadOrphan :: Repository -> IO Bool
isHeadOrphan = repoIs {#call git_repository_head_orphan#}

isEmpty :: Repository -> IO Bool
isEmpty = repoIs {#call git_repository_is_empty#}

path :: Repository -> RepositoryPathID -> IO String
path (Repository r) pathID = return . undefined =<< {#call git_repository_path#} r (fromIntegral $ fromEnum pathID)

isBare :: Repository -> IO Bool
isBare = repoIs {#call git_repository_is_bare#}
