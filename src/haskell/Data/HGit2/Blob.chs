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
-- | Get a read-only buffer with the raw content of a blob.
--
-- A pointer to the raw content of a blob is returned; this pointer is owned
-- internally by the object and shall not be free'd. The pointer may be
-- invalidated at a later time.
rawBlobContent :: Blob -> IO RawData
rawBlobContent (Blob bfp) =
  withForeignPtr bfp $ \b ->
  return . RawData =<< {#call git_blob_rawcontent#} b

-- | Get the size in bytes of the contents of a blob
rawBlobSize :: Blob -> IO Int
rawBlobSize (Blob bfp) =
  withForeignPtr bfp $ \b ->
  retNum $ {#call git_blob_rawsize#} b

-- | Read a file from the working folder of a repository and write it to the
-- Object Database as a loose blob
blobFromFile :: OID -> Repository -> String -> IO (Maybe GitError)
blobFromFile (OID ofp) (Repository rfp) pth =
  withForeignPtr ofp $ \obj ->
  withForeignPtr rfp $ \repo ->
  withCString pth $ \pth' ->
  retMaybe =<< {#call git_blob_create_fromfile #} obj repo pth'

-- | Write an in-memory buffer to the ODB as a blob
blobFromBuffer :: OID -> Repository -> RawData -> IO (Maybe GitError)
blobFromBuffer (OID ofp) (Repository rfp) (RawData buf) =
  withForeignPtr ofp $ \o ->
  withForeignPtr rfp $ \repo ->
  retMaybe =<< {#call git_blob_create_frombuffer#} o repo buf
                                                    (fromIntegral $ sizeOf buf)
