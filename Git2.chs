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
  retEither res $ fmap (Right . Repository) $ peek pprepo

openRepoObjDir :: String -> String -> String -> String -> IO (Either GitError Repository)
openRepoObjDir dir objDir idxFile workTree = alloca $ \pprepo -> do
  dirStr     <- newCString dir
  objDirStr  <- newCString objDir
  idxFileStr <- newCString idxFile
  wtreeStr   <- newCString workTree
  res        <- {#call git_repository_open2#} pprepo dirStr objDirStr idxFileStr wtreeStr
  retEither res $ fmap (Right . Repository) $ peek pprepo

openRepoObjDb :: String -> ObjDB -> String -> String -> IO (Either GitError Repository)
openRepoObjDb dir (ObjDB db) idxFile workTree = alloca $ \pprepo -> do
  dirStr     <- newCString dir
  idxFileStr <- newCString idxFile
  wtreeStr   <- newCString workTree
  res        <- {#call git_repository_open3#} pprepo dirStr db idxFileStr wtreeStr
  retEither res $ fmap (Right . Repository) $ peek pprepo

-- TODO: size? GIT_EXTERN(int) git_repository_discover(char *repository_path, size_t size, const char *start_path, int across_fs, const char *ceiling_dirs);
discover :: String -> Bool -> String -> IO (Either GitError String)
discover startPath acrossFs ceilingDirs = alloca $ \path -> do
  spStr  <- newCString startPath
  cdsStr <- newCString ceilingDirs
  res    <- {#call git_repository_discover#} path undefined spStr (fromBool acrossFs) cdsStr
  retEither res $ undefined

database :: Repository -> IO ObjDB
database (Repository r) = return . ObjDB =<< {#call git_repository_database#} r

index :: Repository -> IO (Either GitError Index)
index (Repository r) = alloca $ \idx -> do
  res  <- {#call git_repository_index#} idx r
  retEither res $ fmap (Right . Index) $ peek idx

free :: Repository -> IO ()
free (Repository r) = {#call git_repository_free#} r

-- TODO: Refactor some bits
init :: String -> Bool -> IO (Either GitError Repository)
init path isBare = alloca $ \pprepo -> do
  pstr <- newCString path
  res  <- {#call git_repository_init#} pprepo pstr (fromBool isBare)
  retEither res $ fmap (Right . Repository) $ peek pprepo

-- TODO: Add tysig
retEither res f | res == 0  = f
                | otherwise = return . Left . toEnum . fromIntegral $ res

isHeadDetached :: Repository -> IO Bool
isHeadDetached = repoIs {#call git_repository_head_detached#}

isHeadOrphan :: Repository -> IO Bool
isHeadOrphan = repoIs {#call git_repository_head_orphan#}

isEmpty :: Repository -> IO Bool
isEmpty = repoIs {#call git_repository_is_empty#}

path :: Repository -> RepositoryPathID -> IO String
path (Repository r) pathID = peekCString =<< {#call git_repository_path#} r p
  where p = fromIntegral $ fromEnum pathID

isBare :: Repository -> IO Bool
isBare = repoIs {#call git_repository_is_bare#}
