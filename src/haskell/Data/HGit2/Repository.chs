{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}

#include <git2/repository.h>

module Data.HGit2.Repository where

import Data.HGit2.Errors
import Data.HGit2.Git2
import Data.HGit2.Index
import Data.HGit2.ODB
import Foreign
import Foreign.C.String
import Foreign.C.Types

newtype Repository = Repository CPtr

instance CWrapper Repository where
  unwrap (Repository r) = r

{#enum git_repository_pathid as RepositoryPathID {underscoreToCase}#}

repoIs :: (CPtr -> IO CInt) -> Repository -> IO Bool
repoIs ffi (Repository ptr) = return . toBool =<< ffi ptr

openRepo :: String -> IO (Either GitError Repository)
openRepo pth = alloca $ \pprepo -> do
  pstr <- newCString pth
  res  <- {#call git_repository_open#} pprepo pstr
  retEither res $ fmap (Right . Repository) $ peek pprepo

openRepoObjDir :: String -> String -> String -> String
               -> IO (Either GitError Repository)
openRepoObjDir dir objDir idxFile workTree = alloca $ \pprepo -> do
  dirStr  <- newCString dir
  objDStr <- newCString objDir
  idxFStr <- newCString idxFile
  wtrStr  <- newCString workTree
  res     <- {#call git_repository_open2#} pprepo dirStr objDStr idxFStr wtrStr
  retEither res $ fmap (Right . Repository) $ peek pprepo

openRepoODB :: String -> ODB -> String -> String
              -> IO (Either GitError Repository)
openRepoODB dir (ODB db) idxFile workTree = alloca $ \pprepo -> do
  dirStr  <- newCString dir
  idxFStr <- newCString idxFile
  wtrStr  <- newCString workTree
  res     <- {#call git_repository_open3#} pprepo dirStr db idxFStr wtrStr
  retEither res $ fmap (Right . Repository) $ peek pprepo

discover :: String -> Bool -> String -> IO (Either GitError String)
discover startPath acrossFs ceilingDirs = alloca $ \pth -> do
  spStr  <- newCString startPath
  cdsStr <- newCString ceilingDirs
  res    <- {#call git_repository_discover#} pth (fromIntegral $ sizeOf pth)
                                             spStr (fromBool acrossFs) cdsStr
  retEither res $ fmap Right $ peekCString pth

database :: Repository -> IO ODB
database = (return . ODB =<<) . {#call git_repository_database#} . unwrap

index :: Repository -> IO (Either GitError Index)
index (Repository r) = alloca $ \idx -> do
  res  <- {#call git_repository_index#} idx r
  retEither res $ fmap (Right . Index) $ peek idx

free :: Repository -> IO ()
free = {#call git_repository_free#} . unwrap

init :: String -> Bool -> IO (Either GitError Repository)
init pth bare = alloca $ \pprepo -> do
  pstr <- newCString pth
  res  <- {#call git_repository_init#} pprepo pstr (fromBool bare)
  retEither res $ fmap (Right . Repository) $ peek pprepo

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
