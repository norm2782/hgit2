{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}

#include <git2/tree.h>

module Data.HGit2.Tree where

import Data.HGit2.Git2
import Data.HGit2.OID
import Data.HGit2.Repository
import Data.HGit2.Errors
import Data.HGit2.Types
import Foreign
import Foreign.C
import System.IO.Unsafe

newtype Tree        = Tree CPtr
newtype TreeEntry   = TreeEntry CPtr
newtype TreeBuilder = TreeBuilder CPtr


ioIntRet :: Integral a => IO a -> IO Int
ioIntRet f = return . fromIntegral =<< f

-- | Get the id of a tree.
treeId :: Tree -> OID
treeId (Tree t) = unsafePerformIO $
  return . OID =<< {#call git_tree_id#} t

-- | Get the number of entries listed in a tree
entryCount :: Tree -> IO Int
entryCount (Tree t) = ioIntRet $ {#call git_tree_entrycount#} t

-- | Lookup a tree entry by its filename
entryByName :: Tree -> String -> IO TreeEntry
entryByName (Tree t) fn = do
  fn' <- newCString fn
  fmap TreeEntry $ {#call git_tree_entry_byname#} t fn'

-- | Lookup a tree entry by its position in the tree
entryByIndex :: Tree -> Int -> IO TreeEntry
entryByIndex (Tree t) n =
  fmap TreeEntry $ {#call git_tree_entry_byindex#} t (fromIntegral n)

-- | Get the UNIX file attributes of a tree entry
attributes :: TreeEntry -> IO Int
attributes (TreeEntry e) = ioIntRet $ {#call git_tree_entry_attributes#} e

-- | Get the filename of a tree entry
name :: TreeEntry -> IO String
name (TreeEntry t) = peekCString =<< {#call git_tree_entry_name#} t

-- | Get the id of the object pointed by the entry
entryId :: TreeEntry -> IO OID
entryId (TreeEntry t) = fmap OID $ {#call git_tree_entry_id#} t

-- | Get the type of the object pointed by the entry
entryType :: TreeEntry -> IO OType
entryType (TreeEntry t) =
  fmap (toEnum . fromIntegral) $ {#call git_tree_entry_type#} t

{-
/**
 * Convert a tree entry to the git_object it points too.
 *
 * @param object pointer to the converted object
 * @param repo repository where to lookup the pointed object
 * @param entry a tree entry
 * @return 0 on success; error code otherwise
 */
GIT_EXTERN(int) git_tree_entry_2object(git_object **object_out, git_repository *repo, const git_tree_entry *entry);

/**
 * Write a tree to the ODB from the index file
 *
 * This method will scan the index and write a representation
 * of its current state back to disk; it recursively creates
 * tree objects for each of the subtrees stored in the index,
 * but only returns the OID of the root tree. This is the OID
 * that can be used e.g. to create a commit.
 *
 * The index instance cannot be bare, and needs to be associated
 * to an existing repository.
 *
 * @param oid Pointer where to store the written tree
 * @param index Index to write
 * @return 0 on success; error code otherwise
 */
GIT_EXTERN(int) git_tree_create_fromindex(git_oid *oid, git_index *index);

/**
 * Create a new tree builder.
 *
 * The tree builder can be used to create or modify
 * trees in memory and write them as tree objects to the
 * database.
 *
 * If the `source` parameter is not NULL, the tree builder
 * will be initialized with the entries of the given tree.
 *
 * If the `source` parameter is NULL, the tree builder will
 * have no entries and will have to be filled manually.
 *
 * @param builder_p Pointer where to store the tree builder
 * @param source Source tree to initialize the builder (optional)
 * @return 0 on sucess; error code otherwise
 */
GIT_EXTERN(int) git_treebuilder_create(git_treebuilder **builder_p, const git_tree *source);
-}

-- | Clear all the entires in the builder
clearTreeBuilder :: TreeBuilder -> IO ()
clearTreeBuilder (TreeBuilder t) = {#call git_treebuilder_clear#} t

-- | Free a tree builder
freeTreeBuilder :: TreeBuilder -> IO ()
freeTreeBuilder (TreeBuilder t) = {#call git_treebuilder_free#} t

{-
/**
 * Get an entry from the builder from its filename
 *
 * The returned entry is owned by the builder and should
 * not be freed manually.
 *
 * @param bld Tree builder
 * @param filename Name of the entry
 * @return pointer to the entry; NULL if not found
 */
GIT_EXTERN(const git_tree_entry *) git_treebuilder_get(git_treebuilder *bld, const char *filename);

/**
 * Add or update an entry to the builder
 *
 * Insert a new entry for `filename` in the builder with the
 * given attributes.
 *
 * if an entry named `filename` already exists, its attributes
 * will be updated with the given ones.
 *
 * The optional pointer `entry_out` can be used to retrieve a
 * pointer to the newly created/updated entry.
 *
 * @param entry_out Pointer to store the entry (optional)
 * @param bld Tree builder
 * @param filename Filename of the entry
 * @param id SHA1 oid of the entry
 * @param attributes Folder attributes of the entry
 * @return 0 on success; error code otherwise
 */
GIT_EXTERN(int) git_treebuilder_insert(git_tree_entry **entry_out, git_treebuilder *bld, const char *filename, const git_oid *id, unsigned int attributes);
-}

-- | Remove an entry from the builder by its filename
removeTreeBuilder :: TreeBuilder -> String -> IO (Maybe GitError)
removeTreeBuilder (TreeBuilder t) fn = do
  str <- newCString fn
  retMaybeRes =<< {#call git_treebuilder_remove#} t str

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
-}

-- | Write the contents of the tree builder as a tree object
writeTreeBuilder :: OID -> Repository -> TreeBuilder -> IO (Maybe GitError)
writeTreeBuilder (OID o) (Repository r) (TreeBuilder t) =
  retMaybeRes =<< {#call git_treebuilder_write#} o r t

