{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}

#include <git2/blob.h>

module Data.HGit2.Blob where

import Data.HGit2.Errors
import Data.HGit2.Git2
import Data.HGit2.OID
import Data.HGit2.Repository
import Data.Maybe ()
import Foreign
import Foreign.C.String
import Foreign.C.Types

newtype Blob = Blob CPtr

instance CWrapper Blob where
  unwrap (Blob b) = b

-- TODO: Could the next two be made "pure"?
rawBlobContent :: Blob -> IO RawData
rawBlobContent (Blob bfp) =
  withForeignPtr bfp $ \b ->
  undefined -- callRetCons {#call git_blob_rawcontent#} RawData

rawBlobSize :: Blob -> IO Int
rawBlobSize (Blob bfp) =
  withForeignPtr bfp $ \b ->
  undefined -- wrapToMNum {#call git_blob_rawsize#}

blobFromFile :: OID -> Repository -> String -> IO (Maybe GitError)
blobFromFile (OID ofp) (Repository rfp) pth =
  withForeignPtr ofp $ \obj ->
  withForeignPtr rfp $ \repo ->
  withCString pth $ \pth' ->
  retMaybe =<< {#call git_blob_create_fromfile #} obj repo pth'

blobFromBuffer :: OID -> Repository -> RawData -> IO (Maybe GitError)
blobFromBuffer (OID ofp) (Repository rfp) (RawData bfp) =
  withForeignPtr ofp $ \o ->
  withForeignPtr rfp $ \repo ->
  withForeignPtr bfp $ \buf ->
  retMaybe =<< {#call git_blob_create_frombuffer#} o repo buf
                                                    (fromIntegral $ sizeOf buf)
