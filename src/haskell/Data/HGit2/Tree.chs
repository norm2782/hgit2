{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}

#include <git2/tree.h>

module Data.HGit2.Tree where

import Data.HGit2.Git2
import Data.HGit2.OID
import Data.HGit2.Repository
import Data.HGit2.Errors
import Data.HGit2.Types
import Data.HGit2.Object
import Data.HGit2.Index
import Foreign
import Foreign.C

newtype Tree        = Tree CPtr
newtype TreeEntry   = TreeEntry CPtr
newtype TreeBuilder = TreeBuilder CPtr

instance CWrapper Tree where
  unwrap (Tree t) = t

instance CWrapper TreeEntry where
  unwrap (TreeEntry te) = te

instance CWrapper TreeBuilder where
  unwrap (TreeBuilder tb) = tb

-- | Get the id of a tree.
treeId :: Tree -> OID
treeId = undefined -- unsafePerformIO . callRetCons {#call git_tree_id#} OID

-- | Get the number of entries listed in a tree
entryCount :: Tree -> IO Int
entryCount (Tree tfp) = withForeignPtr tfp $ \t ->
  retNum $ {#call git_tree_entrycount#} t

-- | Lookup a tree entry by its filename
entryByName :: Tree -> String -> IO TreeEntry
entryByName (Tree tfp) fn =
  withForeignPtr tfp $ \t ->
  withCString fn $ \nm ->
  undefined
  {- fmap TreeEntry $ {#call git_tree_entry_byname#} t nm-}

-- | Lookup a tree entry by its position in the tree
entryByIndex :: Tree -> Int -> IO TreeEntry
entryByIndex (Tree tfp) n =
  withForeignPtr tfp $ \t ->
  undefined
  {- fmap TreeEntry $ {#call git_tree_entry_byindex#} t (fromIntegral n)-}

-- | Get the UNIX file attributes of a tree entry
attributes :: TreeEntry -> IO Int
attributes (TreeEntry tfp) =
  withForeignPtr tfp $ \t ->
  retNum $ {#call git_tree_entry_attributes#} t

-- | Get the filename of a tree entry
name :: TreeEntry -> IO String
name (TreeEntry tfp) =
  withForeignPtr tfp $ \t ->
  peekCString =<< {#call git_tree_entry_name#} t

-- | Get the id of the object pointed by the entry
entryId :: TreeEntry -> IO OID
entryId = undefined -- callRetCons {#call git_tree_entry_id#} OID

-- | Get the type of the object pointed by the entry
entryType :: TreeEntry -> IO OType
entryType (TreeEntry tfp) =
  withForeignPtr tfp $ \t ->
  undefined
  {- fmap (toEnum . fromIntegral) $ {#call git_tree_entry_type#} t-}

-- | Convert a tree entry to the git_object it points too.
entryToObj :: Repository -> TreeEntry -> IOEitherErr GitObj
entryToObj (Repository rfp) (TreeEntry tfp) =
  withForeignPtr rfp $ \r ->
  withForeignPtr tfp $ \t ->
  callPeek GitObj (\out -> {#call git_tree_entry_2object#} out r t)

-- Write a tree to the ODB from the index file
createFromIndex :: OID -> Index -> IO (Maybe GitError)
createFromIndex (OID ofp) (Index ifp) =
  withForeignPtr ofp $ \o ->
  withForeignPtr ifp $ \i ->
  undefined -- retMaybe =<< {#call git_tree_create_fromindex#} o i

-- | Create a new tree builder
createTreeBuilder :: Maybe Tree -> IO (Either GitError TreeBuilder)
createTreeBuilder tr = alloca $ \builder -> do
  res <- {#call git_treebuilder_create#} builder undefined -- t
  undefined
  {- retEither res $ fmap (Right . TreeBuilder) $ peek builder-}
  {- where t = case tr of-}
  {-             Nothing       -> nullPtr-}
  {-             Just (Tree x) -> x-}

-- | Clear all the entires in the builder
clearTreeBuilder :: TreeBuilder -> IO ()
clearTreeBuilder (TreeBuilder tfp) =
  withForeignPtr tfp $ {#call git_treebuilder_clear#}

-- Get an entry from the builder from its filename
getTreeBuilder :: TreeBuilder -> String -> IO (Maybe TreeEntry)
getTreeBuilder (TreeBuilder tfp) fn =
  withForeignPtr tfp $ \b ->
  withCString fn $ \name -> do
  fp <- mkFPtr =<< {#call git_treebuilder_get#} b name
  retRes TreeEntry fp

-- | Add or update an entry to the builder
insertTreeBuilder :: TreeBuilder -> String -> OID -> Int
                  -> IO (Either GitError TreeEntry)
insertTreeBuilder (TreeBuilder tfp) fn (OID ofp) as =
  withForeignPtr tfp $ \b ->
  withForeignPtr ofp $ \o ->
  alloca $ \entry ->
  withCString fn $ \nm -> do
  res <- {#call git_treebuilder_insert#} entry b nm o (fromIntegral as)
  undefined -- retEither res $ fmap (Right . TreeEntry) $ peek entry

-- | Remove an entry from the builder by its filename
removeTreeBuilder :: TreeBuilder -> String -> IO (Maybe GitError)
removeTreeBuilder (TreeBuilder tfp) fn =
  withForeignPtr tfp $ \t ->
  withCString fn $ \nm ->
  undefined -- retMaybe =<< {#call git_treebuilder_remove#} t nm

{-
/**
 * Filter the entries in the tree
 *
 * The `filter` callback will be called for each entry
 * in the tree with a pointer to the entry and the
 * provided `payload`: if the callback returns 1, the
 * entry will be filtered (removed from the builder).
 *
 * @param bld Tree builder
 * @param filter Callback to filter entries
 */
GIT_EXTERN(void) git_treebuilder_filter(git_treebuilder *bld, int (*filter)(const git_tree_entry *, void *), void *payload);
TODO: How to handle callbacks?
-- TODO: what do we make these CPtrs?
-}
-- filterTreeBuilder :: TreeBuilder -> TreeEntry -> CPtr -> CPtr -> IO ()
-- filterTreeBuilder (TreeBuilder b) (TreeEntry e) v p = {#call git_treebuilder_filter#} b -- TODO: What? (FunPtr e v) p

-- | Write the contents of the tree builder as a tree object
writeTreeBuilder :: OID -> Repository -> TreeBuilder -> IO (Maybe GitError)
writeTreeBuilder (OID ofp) (Repository rfp) (TreeBuilder tfp) =
  withForeignPtr ofp $ \o ->
  withForeignPtr rfp $ \r ->
  withForeignPtr tfp $ \t ->
  undefined -- retMaybe =<< {#call git_treebuilder_write#} o r t
