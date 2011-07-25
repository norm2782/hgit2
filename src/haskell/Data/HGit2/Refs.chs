{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}

#include <git2/refs.h>

module Data.HGit2.Refs where

import Data.HGit2.Git2
import Data.HGit2.Repository
import Data.HGit2.OID
import Data.HGit2.Types
import Data.HGit2.Common
import Foreign
import Foreign.C

newtype Reference = Reference CPtr

instance CWrapper Reference where
  unwrap (Reference r) = r

-- | Lookup a reference by its name in a repository.
--
-- The generated reference is owned by the repository and should not be freed
-- by the user.
lookupRef :: Repository -> String -> IOEitherErr Reference
lookupRef (Repository fp) str =
  withForeignPtr fp $ \r ->
    withCString str $ \str' ->
    callPeek Reference (\out -> {#call git_reference_lookup#} out r str')

-- | Create a new symbolic reference.
--
-- The reference will be created in the repository and written to the disk.
--
-- This reference is owned by the repository and shall not be free'd by the
-- user.
--
-- If `force` is true and there already exists a reference with the same name,
-- it will be overwritten.
createSymRef :: Repository -> String -> String -> Bool -> IOEitherErr Reference
createSymRef (Repository fp) n t f =
  withForeignPtr fp $ \r ->
    withCString n $ \nm ->
    withCString t $ \tg ->
    callPeek Reference (\out -> {#call git_reference_create_symbolic#} out r nm
                                                               tg (fromBool f))

-- | Create a new object id reference.
--
-- The reference will be created in the repository and written to the disk.
--
-- This reference is owned by the repository and shall not be free'd by the
-- user.
--
-- If `force` is true and there already exists a reference with the same name,
-- it will be overwritten.
createOID :: Repository -> String -> OID -> Bool -> IOEitherErr Reference
createOID (Repository rfp) n (OID ifp) f =
  withForeignPtr rfp $ \r ->
  withForeignPtr ifp $ \i ->
    withCString n $ \ns -> callPeek Reference
    (\out -> {#call git_reference_create_oid#} out r ns i (fromBool f))

-- | Get the OID pointed to by a reference.
--
-- Only available if the reference is direct (i.e. not symbolic)
refOID :: Reference -> IO (Maybe OID)
refOID (Reference rfp) =
  withForeignPtr rfp $ \ref -> do
  p <- mkFPtr =<< {#call git_reference_oid#} ref
  retRes OID p

-- | Get full name to the reference pointed by this reference
--
-- Only available if the reference is symbolic
refTarget :: Reference -> IO String
refTarget (Reference rfp) =
  withForeignPtr rfp $ \ref ->
  peekCString =<< {#call git_reference_target#} ref

-- | Get the type of a reference
--
-- Either direct (GIT_REF_OID) or symbolic (GIT_REF_SYMBOLIC)
refType :: Reference -> IO RType
refType (Reference rfp) =
  withForeignPtr rfp $ \ref ->
  return . toEnum . fromIntegral =<< {#call git_reference_type#} ref

-- | Get the full name of a reference
refName :: Reference -> IO String
refName (Reference rfp) =
  withForeignPtr rfp $ \ref ->
  peekCString =<< {#call git_reference_name#} ref

-- | Resolve a symbolic reference
--
-- Thie method iteratively peels a symbolic reference until it resolves to a
-- direct reference to an OID.
--
-- If a direct reference is passed as an argument, that reference is returned
-- immediately
resolveRef :: Reference -> IOEitherErr Reference
resolveRef (Reference rfp) =
  withForeignPtr rfp $ \r ->
  callPeek Reference (\out -> {#call git_reference_resolve#} out r)

-- | Get the repository where a reference resides
refOwner :: Reference -> IO Repository
refOwner (Reference rfp) =
  withForeignPtr rfp $ \r -> do
  ptr <- mkFPtr =<< {#call git_reference_owner#} r
  return $ Repository ptr

-- | Set the symbolic target of a reference.
--
-- The reference must be a symbolic reference, otherwise this method will fail.
--
-- The reference will be automatically updated in memory and on disk.
setRefTarget :: Reference -> String -> IOCanFail
setRefTarget (Reference rfp) s =
  withForeignPtr rfp $ \r ->
  withCString s $ \s' ->
  retMaybe =<< {#call git_reference_set_target#} r s'

-- | Set the OID target of a reference.
--
-- The reference must be a direct reference, otherwise this method will fail.
--
-- The reference will be automatically updated in memory and on disk.
setRefOID :: Reference -> OID -> IOCanFail
setRefOID (Reference rfp) (OID ofp) =
  withForeignPtr rfp $ \r ->
  withForeignPtr ofp $ \o ->
  retMaybe =<< {#call git_reference_set_oid#} r o

-- | Rename an existing reference
--
-- This method works for both direct and symbolic references. The new name will
-- be checked for validity and may be modified into a normalized form.
--
-- The refernece will be immediately renamed in-memory and on disk.
renameRef :: Reference -> String -> Bool -> IOCanFail
renameRef (Reference rfp) s b =
  withForeignPtr rfp $ \r ->
  withCString s $ \str ->
  retMaybe =<< {#call git_reference_rename#} r str (fromBool b)

-- | Delete an existing reference
--
-- This method works for both direct and symbolic references.
--
-- The reference will be immediately removed on disk and from memory. The given
-- reference pointer will no longer be valid.
delRef :: Reference -> IOCanFail
delRef (Reference rfp) =
  withForeignPtr rfp $ \r ->
  retMaybe =<< {#call git_reference_delete#} r

-- | Pack all the loose references in the repository
--
-- This method will load into the cache all the loose references on the
-- repository and update the `packed-refs` file with them.
--
-- Once the `packed-refs` file has been written properly, the loose references
-- will be removed from disk.
--
-- WARNING: calling this method may invalidate any existing references
-- previously loaded on the cache.
packAllRef :: Repository -> IOCanFail
packAllRef (Repository rfp) =
  withForeignPtr rfp $ \r ->
  retMaybe =<< {#call git_reference_packall#} r

-- | Fill a list with all the references that can be found in a repository.
--
-- The listed references may be filtered by type, or using a bitwise OR of
-- several types. Use the magic value `GIT_REF_LISTALL` to obtain all
-- references, including packed ones.
--
-- The string array will be filled with the names of all references; these
-- values are owned by the user and should be free'd manually when no longer
-- needed, using `git_strarray_free`.
listAllRef :: StrArray -> Repository -> Int -> IOCanFail
listAllRef (StrArray sfp) (Repository rfp) n =
  withForeignPtr sfp $ \s ->
  withForeignPtr rfp $ \r ->
  retMaybe =<< {#call git_reference_listall#} s r (fromIntegral n)

{-
/**
 * Perform an operation on each reference in the repository
 *
 * The processed references may be filtered by type, or using
 * a bitwise OR of several types. Use the magic value
 * `GIT_REF_LISTALL` to obtain all references, including
 * packed ones.
 *
 * The `callback` function will be called for each of the references
 * in the repository, and will receive the name of the reference and
 * the `payload` value passed to this method.
 *
 * @param repo Repository where to find the refs
 * @param list_flags Filtering flags for the reference
 *		listing.
 * @param callback Function which will be called for every listed ref
 * @param payload Additional data to pass to the callback
 * @return 0 on success; error code otherwise
 */
GIT_EXTERN(int) git_reference_foreach(git_repository *repo, unsigned int list_flags, int (*callback)(const char *, void *), void *payload);
-}
foreachRef = undefined
