{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}

#include <git2/object.h>

module Data.HGit2.Object where

import Data.HGit2.Types
import Data.HGit2.Git2
import Data.HGit2.OID
import Data.HGit2.Repository
import Foreign
import Foreign.C.Types
import Foreign.C.String

newtype GitObj = GitObj CPtr

instance CWrapper GitObj where
  unwrap (GitObj o) = o

-- | Lookup a reference to one of the objects in a repostory.
lookupObj :: Repository -> OID -> OType -> IOEitherErr GitObj
lookupObj (Repository fp) (OID ofp) oty =
  withForeignPtr fp $ \r ->
  withForeignPtr ofp $ \o ->
  callPeek GitObj (\out -> {#call git_object_lookup#} out r o
    (fromIntegral $ fromEnum oty))

-- | Lookup a reference to one of the objects in a repostory, given a prefix of
-- its identifier (short id).
-- TODO: Calculate size of OID dynamically, rather than passing an int
lookupObjPref :: Repository -> OID -> Int -> OType -> IOEitherErr GitObj
lookupObjPref (Repository fp) (OID ofp) n oty =
  withForeignPtr fp $ \r ->
  withForeignPtr ofp $ \o ->
  callPeek GitObj (\out -> {#call git_object_lookup_prefix#} out r o
    (fromIntegral n) (fromIntegral $ fromEnum oty))

-- | Get the id (SHA1) of a repository object
oid :: GitObj -> OID
oid (GitObj ofp) = unsafePerformIO $
  withForeignPtr ofp $ \o -> do
  ptr <- mkFPtr =<< {#call unsafe git_object_id#} o
  return $ OID ptr

-- | Get the object type of an object
objTy :: GitObj -> OType
objTy (GitObj ofp) = unsafePerformIO $
  withForeignPtr ofp $ retEnum . {#call unsafe git_object_type#}

-- | Get the repository that owns this object
--
-- Freeing or calling `git_repository_close` on the returned pointer will
-- invalidate the actual object.
--
-- Any other operation may be run on the repository without affecting the
-- object.
-- TODO: Refactor
objOwner :: GitObj -> Repository
objOwner (GitObj ofp) = unsafePerformIO $
  withForeignPtr ofp $ \o -> do
  ptr <- mkFPtr =<< {#call git_object_owner#} o
  return $ Repository ptr

-- | Close an open object
--
-- This method instructs the library to close an existing object; note that
-- GitObjs are owned and cached by the repository so the object may or may not
-- be freed after this library call, depending on how agressive is the caching
-- mechanism used by the repository.
--
-- IMPORTANT:
-- It *is* necessary to call this method when you stop using an object. Failure
-- to do so will cause a memory leak.
closeObj :: GitObj -> IO ()
closeObj (GitObj gfp) =
  withForeignPtr gfp $ {#call git_object_close#}

-- | Convert an object type to it's string representation.
oTypeToString :: OType -> IO String
oTypeToString oty =
  peekCString =<< {#call git_object_type2string#} (fromIntegral $ fromEnum oty)

-- | Convert a string object type representation to it's git_otype.
strToOType :: String -> IO OType
strToOType str = withCString str $ retEnum . {#call git_object_string2type#}

-- | Determine if the given git_otype is a valid loose object type.
isLoose :: OType -> Bool
isLoose oty = unsafePerformIO $ return . toBool =<<
  {#call git_object_typeisloose#} (fromIntegral $ fromEnum oty)

-- | Get the size in bytes for the structure which acts as an in-memory
-- representation of any given object type.
objSize :: OType -> IO Integer
objSize = retNum . {#call git_object__size#} . (fromIntegral . fromEnum)

