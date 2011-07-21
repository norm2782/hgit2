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

openRepo :: String -> IOEitherErr Repository
openRepo pth = do
  pstr <- newCString pth
  callPeek Repository (\out -> {#call git_repository_open#} out pstr)

openRepoObjDir :: String -> String -> String -> String -> IOEitherErr Repository
openRepoObjDir dir objDir idxFile workTree = do
  dirStr  <- newCString dir
  objDStr <- newCString objDir
  idxFStr <- newCString idxFile
  wtrStr  <- newCString workTree
  callPeek Repository (\out -> {#call git_repository_open2#} out dirStr objDStr idxFStr wtrStr)

openRepoODB :: String -> ODB -> String -> String -> IOEitherErr Repository
openRepoODB dir (ODB db) idxFile workTree = do
  dirStr  <- newCString dir
  idxFStr <- newCString idxFile
  wtrStr  <- newCString workTree
  callPeek Repository (\out -> {#call git_repository_open3#} out dirStr db idxFStr wtrStr)

discover :: String -> Bool -> String -> IOEitherErr String
discover startPath acrossFs ceilingDirs = alloca $ \out -> do
  spStr  <- newCString startPath
  cdsStr <- newCString ceilingDirs
  res    <- {#call git_repository_discover#} out undefined
                                             spStr (fromBool acrossFs) cdsStr
  eitherPeekStr out id res

database :: Repository -> IO ODB
database = callRetCons {#call git_repository_database#} ODB

index :: Repository -> IOEitherErr Index
index (Repository r) = callPeek Index
  (\out -> {#call git_repository_index#} out r)

free :: Repository -> IO ()
free = {#call git_repository_free#} . unwrap

init :: String -> Bool -> IOEitherErr Repository
init pth bare = do
  pstr <- newCString pth
  callPeek Repository (\out -> {#call git_repository_init#} out pstr (fromBool bare))

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
