{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}

#include "git2.h"

module Data.HGit2.Blob where

import Data.Bits
import Data.HGit2.Git2
import Data.Maybe
import Foreign
import Foreign.C.String
import Foreign.C.Types

{-
/**
 * Get a read-only buffer with the raw content of a blob.
 *
 * A pointer to the raw content of a blob is returned;
 * this pointer is owned internally by the object and shall
 * not be free'd. The pointer may be invalidated at a later
 * time.
 *
 * @param blob pointer to the blob
 * @return the pointer; NULL if the blob has no contents
 */
GIT_EXTERN(const void *) git_blob_rawcontent(git_blob *blob);
-}
-- TODO: Could the next two be made "pure"
-- TODO: Figure out what this function should return...
rawBlobContent :: Blob -> IO CPtr
rawBlobContent (Blob b) = {#call git_blob_rawcontent#} b

rawBlobSize :: Blob -> IO Int
rawBlobSize (Blob b) = return . fromIntegral =<< {#call git_blob_rawsize#} b

blobFromFile :: ObjID -> Repository -> String -> IO (Maybe GitError)
blobFromFile (ObjID obj) (Repository repo) path = do
  pathStr  <- newCString path
  res      <- {#call git_blob_create_fromfile #} obj repo pathStr
  retMaybeRes res

-- TODO: CPtr here?
blobFromBuffer :: ObjID -> Repository -> CPtr -> IO (Maybe GitError)
blobFromBuffer (ObjID objId) (Repository repo) buf = do
  res <- {#call git_blob_create_frombuffer#} objId repo buf
                                             (fromIntegral $ sizeOf buf)
  retMaybeRes res