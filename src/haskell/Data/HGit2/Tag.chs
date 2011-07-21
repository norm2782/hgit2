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
import Foreign
import Foreign.C

newtype Tag = Tag CPtr
newtype Target = Target CPtr
newtype StrArray = StrArray CPtr

instance CWrapper Tag where
  unwrap (Tag t) = t

instance CWrapper Target where
  unwrap (Target t) = t

instance CWrapper StrArray where
  unwrap (StrArray a) = a

-- | Get the id of a tag.
tagID :: Tag -> IO OID
tagID = callRetCons {#call git_tag_id#} OID

-- | Get the tagged object of a tag
--
-- This method performs a repository lookup for the given object and returns it
target :: Tag -> IOEitherErr Target
target (Tag t) = callPeek Target (\out -> {#call git_tag_target#} out t)

-- | Get the OID of the tagged object of a tag
targetOID :: Tag -> IO OID
targetOID = callRetCons {#call git_tag_target_oid#} OID

-- | Get the type of a tag's tagged object
tagType :: Tag -> OType
tagType = unsafePerformIO . retEnum . {#call git_tag_type#} . unwrap

-- | Get the name of a tag
tagName :: Tag -> String
tagName = unsafePeekStr {#call git_tag_name#}

-- | Get the tagger (author) of a tag
tagger :: Tag -> Signature
tagger = unsafePerformIO . callRetCons {#call git_tag_tagger#} Signature

-- | Get the message of a tag
tagMessage :: Tag -> String
tagMessage = unsafePeekStr {#call git_tag_message#}

-- | Create a new tag in the repository from an object
--
-- A new reference will also be created pointing to this tag object. If `force`
-- is true and a reference already exists with the given name, it'll be
-- replaced.
createTag :: OID -> Repository -> String -> GitObj -> Signature -> String
          -> Bool -> IOCanFail
createTag (OID o) (Repository r) tg (GitObj g) (Signature s) ms fr = do
  tag <- newCString tg
  msg <- newCString ms
  retMaybe =<< {#call git_tag_create#} o r tag g s msg (fromBool fr)

-- | Create a new tag in the repository from a buffer
createFromBuff :: OID -> Repository -> String -> Bool -> IOCanFail
createFromBuff (OID o) (Repository r) bf fr = do
  buf <- newCString bf
  retMaybe =<< {#call git_tag_create_frombuffer#} o r buf (fromBool fr)

-- | Create a new lightweight tag pointing at a target object
--
-- A new direct reference will be created pointing to this target object. If
-- `force` is true and a reference already exists with the given name, it'll be
-- replaced.
createLightWeight :: OID -> Repository -> String -> GitObj -> Bool -> IOCanFail
createLightWeight (OID o) (Repository r) tn (GitObj g) fr = do
  tag <- newCString tn
  retMaybe =<< {#call git_tag_create_lightweight#} o r tag g (fromBool fr)

-- | Delete an existing tag reference.
deleteTag :: Repository -> String -> IOCanFail
deleteTag (Repository r) tn =
  retMaybe =<< {#call git_tag_delete#} r =<< newCString tn

-- | Fill a list with all the tags in the Repository
--
-- The string array will be filled with the names of the matching tags; these
-- values are owned by the user and should be free'd manually when no longer
-- needed, using `git_strarray_free`.
tagList :: StrArray -> Repository -> IOCanFail
tagList (StrArray sa) (Repository r) = retMaybe =<< {#call git_tag_list#} sa r

-- | Fill a list with all the tags in the Repository which name match a defined
-- pattern
--
-- If an empty pattern is provided, all the tags will be returned.
--
-- The string array will be filled with the names of the matching tags; these
-- values are owned by the user and should be free'd manually when no longer
-- needed, using `git_strarray_free`.
tagListMatch :: StrArray -> String -> Repository -> IOCanFail
tagListMatch (StrArray s) pt (Repository r) = do
  pat <- newCString pt
  retMaybe =<< {#call git_tag_list_match#} s pat r
