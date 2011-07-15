{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE StandaloneDeriving #-}

#include "git2.h"

module Git2 where

import Data.Primitive
import Foreign
import Foreign.C.String
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

deriving instance Show GitError

repoIs :: Ptr2Int -> Repository -> IO Bool
repoIs ffi (Repository ptr) = return . toBool =<< ffi ptr

openRepo :: String -> IO (Either GitError Repository)
openRepo path = alloca $ \pprepo -> do
  pstr <- newCString path
  res  <- {#call git_repository_open#} pprepo pstr
  if res == 0
    then fmap (Right . Repository) $ peek pprepo
    else return . Left . toEnum . fromIntegral $ res

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
index (Repository r) = alloca $ \idx -> do
  res  <- {#call git_repository_index#} idx r
  if res == 0
    then fmap (Right . Index) $ peek idx
    else return . Left . toEnum . fromIntegral $ res

free :: Repository -> IO ()
free (Repository r) = {#call git_repository_free#} r

-- TODO: Refactor some bits
init :: String -> Bool -> IO (Either GitError Repository)
init path isBare = alloca $ \pprepo -> do
  pstr <- newCString path
  res  <- {#call git_repository_init#} pprepo pstr (fromBool isBare)
  if res == 0
    then fmap (Right . Repository) $ peek pprepo
    else return . Left . toEnum . fromIntegral $ res

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