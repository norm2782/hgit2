{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}

#include <git2/object.h>

module Data.HGit2.Object where

import Data.HGit2.Errors
import Data.HGit2.Types
import Data.HGit2.Git2
import Data.HGit2.OID
import Data.HGit2.Repository
import Foreign
import Foreign.C.Types

newtype GitObj = GitObj CPtr

-- | Lookup a reference to one of the objects in a repostory.
lookupObj :: Repository -> OID -> OType -> IO (Either GitError GitObj)
lookupObj (Repository r) (OID oid) oty = alloca $ \obj -> do
  res <- {#call git_object_lookup#} obj r oid (fromIntegral $ fromEnum oty)
  retEither res $ fmap (Right . GitObj) $ peek obj

-- | Lookup a reference to one of the objects in a repostory, given a prefix of
-- its identifier (short id).
-- TODO: Calculate size of OID dynamically, rather than passing an int
lookupObjPref :: Repository -> OID -> Int -> OType
              -> IO (Either GitError GitObj)
lookupObjPref (Repository r) (OID o) n oty = alloca $ \obj -> do
  res <- {#call git_object_lookup_prefix#} obj r o (fromIntegral n)
                                           (fromIntegral $ fromEnum oty)
  retEither res $ fmap (Right . GitObj) $ peek obj

-- | Get the id (SHA1) of a repository object
oid :: GitObj -> OID
oid (GitObj go) = unsafePerformIO $
  return . OID =<< {#call unsafe git_object_id#} go

-- | Get the object type of an object
objTy :: GitObj -> OType
objTy (GitObj go) = unsafePerformIO $
  return . toEnum . fromIntegral =<< {#call unsafe git_object_type#} go

{-
/**
 * Get the repository that owns this object
 *
 * Freeing or calling `git_repository_close` on the
 * returned pointer will invalidate the actual object.
 *
 * Any other operation may be run on the repository without
 * affecting the object.
 *
 * @param obj the object
 * @return the repository who owns this object
 */
GIT_EXTERN(git_repository *) git_object_owner(const git_object *obj);

/**
 * Close an open object
 *
 * This method instructs the library to close an existing
 * object; note that git_objects are owned and cached by the repository
 * so the object may or may not be freed after this library call,
 * depending on how agressive is the caching mechanism used
 * by the repository.
 *
 * IMPORTANT:
 * It *is* necessary to call this method when you stop using
 * an object. Failure to do so will cause a memory leak.
 *
 * @param object the object to close
 */
GIT_EXTERN(void) git_object_close(git_object *object);

/**
 * Convert an object type to it's string representation.
 *
 * The result is a pointer to a string in static memory and
 * should not be free()'ed.
 *
 * @param type object type to convert.
 * @return the corresponding string representation.
 */
GIT_EXTERN(const char *) git_object_type2string(git_otype type);

/**
 * Convert a string object type representation to it's git_otype.
 *
 * @param str the string to convert.
 * @return the corresponding git_otype.
 */
GIT_EXTERN(git_otype) git_object_string2type(const char *str);

/**
 * Determine if the given git_otype is a valid loose object type.
 *
 * @param type object type to test.
 * @return true if the type represents a valid loose object type,
 * false otherwise.
 */
GIT_EXTERN(int) git_object_typeisloose(git_otype type);

/**
 * Get the size in bytes for the structure which
 * acts as an in-memory representation of any given
 * object type.
 *
 * For all the core types, this would the equivalent
 * of calling `sizeof(git_commit)` if the core types
 * were not opaque on the external API.
 *
 * @param type object type to get its size
 * @return size in bytes of the object
 */
GIT_EXTERN(size_t) git_object__size(git_otype type);
-}
