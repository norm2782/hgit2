{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}

#include <git2/tag.h>

module Data.HGit2.Tag where

import Data.HGit2.Git2
import Data.HGit2.Signature
import Data.HGit2.OID
import Data.HGit2.Types
import Data.HGit2.Repository
import Data.HGit2.Object
import Data.HGit2.Common
import Foreign
import Foreign.C

newtype Tag = Tag CPtr
newtype Target = Target CPtr

instance CWrapper Tag where
  unwrap (Tag t) = t

instance CWrapper Target where
  unwrap (Target t) = t

-- | Get the id of a tag.
tagID :: Tag -> IO OID
tagID (Tag tfp) =
  withForeignPtr tfp $ \t -> do
  ptr <- mkFPtr =<< {#call git_tag_id#} t
  return $ OID ptr

-- | Get the tagged object of a tag
--
-- This method performs a repository lookup for the given object and returns it
target :: Tag -> IOEitherErr Target
target (Tag tfp) =
  withForeignPtr tfp $ \t ->
  callPeek Target (\out -> {#call git_tag_target#} out t)

-- | Get the OID of the tagged object of a tag
targetOID :: Tag -> IO OID
targetOID (Tag tfp) =
  withForeignPtr tfp $ \t -> do
  ptr <- mkFPtr =<< {#call git_tag_target_oid#} t
  return $ OID ptr

-- | Get the type of a tag's tagged object
tagType :: Tag -> OType
tagType (Tag tfp) = unsafePerformIO $
  retEnum $ withForeignPtr tfp $ {#call git_tag_type#}

-- | Get the name of a tag
tagName :: Tag -> String
tagName (Tag tfp) = unsafePerformIO $
  withForeignPtr tfp $ \t ->
  peekCString =<< {#call git_tag_name#} t

-- | Get the tagger (author) of a tag
tagger :: Tag -> Signature
tagger (Tag tfp) = unsafePerformIO $
  withForeignPtr tfp $ \t -> do
  ptr <- mkFPtr =<< {#call git_tag_tagger#} t
  return $ Signature ptr

-- | Get the message of a tag
tagMessage :: Tag -> String
tagMessage (Tag tfp) = unsafePerformIO $
  withForeignPtr tfp $ \t ->
  peekCString =<< {#call git_tag_message#} t

-- | Create a new tag in the repository from an object
--
-- A new reference will also be created pointing to this tag object. If `force`
-- is true and a reference already exists with the given name, it'll be
-- replaced.
createTag :: OID -> Repository -> String -> GitObj -> Signature -> String
          -> Bool -> IOCanFail
createTag (OID ofp) (Repository rfp) tg (GitObj gfp) (Signature sfp) ms fr =
  withForeignPtr ofp $ \o ->
  withForeignPtr rfp $ \r ->
  withForeignPtr gfp $ \g ->
  withForeignPtr sfp $ \s ->
  withCString tg $ \tag ->
  withCString ms $ \msg ->
  retMaybe =<< {#call git_tag_create#} o r tag g s msg (fromBool fr)

-- | Create a new tag in the repository from a buffer
createFromBuff :: OID -> Repository -> String -> Bool -> IOCanFail
createFromBuff (OID ofp) (Repository rfp) bf fr =
  withForeignPtr ofp $ \o ->
  withForeignPtr rfp $ \r ->
  withCString bf $ \buf ->
  retMaybe =<< {#call git_tag_create_frombuffer#} o r buf (fromBool fr)

-- | Create a new lightweight tag pointing at a target object
--
-- A new direct reference will be created pointing to this target object. If
-- `force` is true and a reference already exists with the given name, it'll be
-- replaced.
createLightWeight :: OID -> Repository -> String -> GitObj -> Bool -> IOCanFail
createLightWeight (OID ofp) (Repository rfp) tn (GitObj gfp) fr =
  withForeignPtr ofp $ \o ->
  withForeignPtr rfp $ \r ->
  withForeignPtr gfp $ \g ->
  withCString tn $ \tag ->
  retMaybe =<< {#call git_tag_create_lightweight#} o r tag g (fromBool fr)

-- | Delete an existing tag reference.
deleteTag :: Repository -> String -> IOCanFail
deleteTag (Repository rfp) tn =
  withForeignPtr rfp $ \r ->
  withCString tn $ \tag ->
  retMaybe =<< {#call git_tag_delete#} r tag

-- | Fill a list with all the tags in the Repository
--
-- The string array will be filled with the names of the matching tags; these
-- values are owned by the user and should be free'd manually when no longer
-- needed, using `git_strarray_free`.
tagList :: StrArray -> Repository -> IOCanFail
tagList (StrArray sfp) (Repository rfp) =
  withForeignPtr sfp $ \sa ->
  withForeignPtr rfp $ \r ->
  retMaybe =<< {#call git_tag_list#} sa r

-- | Fill a list with all the tags in the Repository which name match a defined
-- pattern
--
-- If an empty pattern is provided, all the tags will be returned.
--
-- The string array will be filled with the names of the matching tags; these
-- values are owned by the user and should be free'd manually when no longer
-- needed, using `git_strarray_free`.
tagListMatch :: StrArray -> String -> Repository -> IOCanFail
tagListMatch  (StrArray sfp) pt (Repository rfp) =
  withForeignPtr sfp $ \s ->
  withForeignPtr rfp $ \r ->
  withCString pt $ \pat ->
  retMaybe =<< {#call git_tag_list_match#} s pat r
