{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE StandaloneDeriving #-}

#include "git2.h"

module Git2 where

import Data.Bits
import Data.Maybe
import Foreign
import Foreign.C.String
import Foreign.C.Types
import System.IO.Unsafe

type OffT    = {#type git_off_t#}
type TimeT   = {#type git_time_t#}
type CPtr    = Ptr ()
type Ptr2Int = CPtr -> IO CInt

newtype ObjDB      = ObjDB CPtr
newtype Repository = Repository CPtr
newtype Index      = Index CPtr
newtype Blob       = Blob CPtr
newtype ObjID      = ObjID CPtr
newtype Commit     = Commit CPtr
newtype Signature  = Signature CPtr
newtype Tree       = Tree CPtr
newtype Config     = Config CPtr
newtype IndexEntry = IndexEntry CPtr
newtype IndexEntryUnMerged = IndexEntryUnMerged CPtr
newtype GitObj     = GitObj CPtr

defaultPort :: String
defaultPort = "9418" -- TODO: Import from net.h?

{#enum define Direction { GIT_DIR_FETCH as Fetch
                        , GIT_DIR_PUSH as Push}#}

{-
#define GIT_STATUS_CURRENT        0
/** Flags for index status */
#define GIT_STATUS_INDEX_NEW      (1 << 0)
#define GIT_STATUS_INDEX_MODIFIED (1 << 1)
#define GIT_STATUS_INDEX_DELETED  (1 << 2)

/** Flags for worktree status */
#define GIT_STATUS_WT_NEW         (1 << 3)
#define GIT_STATUS_WT_MODIFIED    (1 << 4)
#define GIT_STATUS_WT_DELETED     (1 << 5)

// TODO Ignored files not handled yet
#define GIT_STATUS_IGNORED        (1 << 6)
-}

{-
{#enum define Status { GIT_STATUS_CURRENT as Current
                     , GIT_STATUS_INDEX_NEW as NewIndex
                     , GIT_STATUS_INDEX_MODIFIED as ModifiedIndex
                     , GIT_STATUS_INDEX_DELETED as DeletedIndex
                     , GIT_STATUS_WT_NEW as NewWorkTree
                     , GIT_STATUS_WT_MODIFIED as ModifiedWorkTree
                     , GIT_STATUS_WT_DELETED as DeletedWorkTree
                     , GIT_STATUS_IGNORED as Ignored}#}
-}

{#enum git_otype as OType {underscoreToCase}#}
{#enum git_rtype as RType {underscoreToCase}#}
{#enum git_repository_pathid as RepositoryPathID {underscoreToCase}#}
{#enum git_error as GitError {underscoreToCase}#}
{#enum git_odb_streammode as ODBStreamMode {underscoreToCase}#}

deriving instance Show GitError



retEither :: CInt -> IO (Either GitError a) -> IO (Either GitError a)
retEither res f | res == 0  = f
                | otherwise = return . Left . toEnum . fromIntegral $ res


retMaybeRes :: CInt -> IO (Maybe GitError)
retMaybeRes res | res == 0  = return Nothing
                | otherwise = return $ Just . toEnum . fromIntegral $ res


-------------------------------------------------------------------------------
-- BEGIN: blob.h
-------------------------------------------------------------------------------

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

-------------------------------------------------------------------------------
-- END: blob.h
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- BEGIN: commit.h
-------------------------------------------------------------------------------

commitId :: Commit -> ObjID
commitId (Commit c) = unsafePerformIO $
  return . ObjID =<< {#call unsafe git_commit_id#} c

shortCommitMsg :: Commit -> String
shortCommitMsg (Commit c) = unsafePerformIO $
  peekCString =<< {#call unsafe git_commit_message_short#} c

commitMsg :: Commit -> String
commitMsg (Commit c) = unsafePerformIO $
  peekCString =<< {#call unsafe git_commit_message#} c

commitTime :: Commit -> TimeT
commitTime (Commit c) = unsafePerformIO $
  return =<< {#call unsafe git_commit_time#} c

timeOffset :: Commit -> Int
timeOffset (Commit c) = unsafePerformIO $
  return . fromIntegral =<< {#call unsafe git_commit_time_offset#} c

committer :: Commit -> Signature
committer (Commit c) = unsafePerformIO $
  return . Signature =<< {#call unsafe git_commit_committer#} c

author :: Commit -> Signature
author (Commit c) = unsafePerformIO $
  return . Signature =<< {#call unsafe git_commit_author#} c

tree :: Commit -> IO (Either GitError Tree)
tree (Commit c) = alloca $ \tree -> do
  res <- {#call git_commit_tree#} tree c
  retEither res $ fmap (Right . Tree) $ peek tree

treeOid :: Commit -> ObjID
treeOid (Commit c) = unsafePerformIO $
  return . ObjID =<< {#call unsafe git_commit_tree_oid#} c

parentCount :: Commit -> IO Int
parentCount (Commit c) =
  return . fromIntegral =<< {#call unsafe git_commit_parentcount#} c

parent :: Commit -> Int -> IO (Either GitError Commit)
parent (Commit c) n = alloca $ \parent -> do
  res <- {#call git_commit_parent#} parent c (fromIntegral n)
  retEither res $ fmap (Right . Commit) $ peek parent

parentObjID :: Commit -> Int -> IO (Maybe ObjID)
parentObjID (Commit c) n = do
  res <- {#call git_commit_parent_oid#} c (fromIntegral n)
  if res == nullPtr
    then return Nothing
    else return . Just . ObjID $ res

createCommit :: ObjID -> Repository -> Maybe String -> Signature -> Signature
             -> String -> Tree -> [Commit] -> IO (Maybe GitError)
createCommit (ObjID objId) (Repository r) mref (Signature ausig)
             (Signature comsig) msg (Tree t) ps = do
  updRef <- case mref of
              Nothing -> return nullPtr
              Just x  -> newCString x
  msgStr <- newCString msg
  carr   <- newArray [c | Commit c <- ps]
  res    <- {#call git_commit_create#} objId r updRef ausig comsig msgStr t cnt carr
  retMaybeRes res
  where cnt = fromIntegral $ length ps

-------------------------------------------------------------------------------
-- END: commit.h
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- BEGIN: common.h
-------------------------------------------------------------------------------

-- TODO: Support?
-- GIT_EXTERN(void) git_strarray_free(git_strarray *array);
{-
/**
 * Return the version of the libgit2 library
 * being currently used.
 *
 * @param major Store the major version number
 * @param minor Store the minor version number
 * @param rev Store the revision (patch) number
 */
GIT_EXTERN(void) git_libgit2_version(int *major, int *minor, int *rev);
-}

-------------------------------------------------------------------------------
-- END: common.h
-------------------------------------------------------------------------------



-------------------------------------------------------------------------------
-- BEGIN: config.h
-------------------------------------------------------------------------------

{-
/**
 * Locate the path to the global configuration file
 *
 * The user or global configuration file is usually
 * located in `$HOME/.gitconfig`.
 *
 * This method will try to guess the full path to that
 * file, if the file exists. The returned path
 * may be used on any `git_config` call to load the
 * global configuration file.
 *
 * @param global_config_path Buffer of GIT_PATH_MAX length to store the path
 * @return GIT_SUCCESS if a global configuration file has been
 *	found. Its path will be stored in `buffer`.
 */
GIT_EXTERN(int) git_config_find_global(char *global_config_path);

/**
 * Open the global configuration file
 *
 * Utility wrapper that calls `git_config_find_global`
 * and opens the located file, if it exists.
 *
 * @param out Pointer to store the config instance
 * @return GIT_SUCCESS on success; error code otherwise
 */
GIT_EXTERN(int) git_config_open_global(git_config **out);

/**
 * Create a configuration file backend for ondisk files
 *
 * These are the normal `.gitconfig` files that Core Git
 * processes. Note that you first have to add this file to a
 * configuration object before you can query it for configuration
 * variables.
 *
 * @param out the new backend
 * @param path where the config file is located
 */
GIT_EXTERN(int) git_config_file__ondisk(struct git_config_file **out, const char *path);

/**
 * Allocate a new configuration object
 *
 * This object is empty, so you have to add a file to it before you
 * can do anything with it.
 *
 * @param out pointer to the new configuration
 * @return GIT_SUCCESS on success; error code otherwise
 */
GIT_EXTERN(int) git_config_new(git_config **out);

/**
 * Add a generic config file instance to an existing config
 *
 * Note that the configuration object will free the file
 * automatically.
 *
 * Further queries on this config object will access each
 * of the config file instances in order (instances with
 * a higher priority will be accessed first).
 *
 * @param cfg the configuration to add the file to
 * @param file the configuration file (backend) to add
 * @param priority the priority the backend should have
 * @return GIT_SUCCESS on success; error code otherwise
 */
GIT_EXTERN(int) git_config_add_file(git_config *cfg, git_config_file *file, int priority);

/**
 * Add an on-disk config file instance to an existing config
 *
 * The on-disk file pointed at by `path` will be opened and
 * parsed; it's expected to be a native Git config file following
 * the default Git config syntax (see man git-config).
 *
 * Note that the configuration object will free the file
 * automatically.
 *
 * Further queries on this config object will access each
 * of the config file instances in order (instances with
 * a higher priority will be accessed first).
 *
 * @param cfg the configuration to add the file to
 * @param path path to the configuration file (backend) to add
 * @param priority the priority the backend should have
 * @return GIT_SUCCESS on success; error code otherwise
 */
GIT_EXTERN(int) git_config_add_file_ondisk(git_config *cfg, const char *path, int priority);


/**
 * Create a new config instance containing a single on-disk file
 *
 * This method is a simple utility wrapper for the following sequence
 * of calls:
 *	- git_config_new
 *	- git_config_add_file_ondisk
 *
 * @param cfg The configuration instance to create
 * @param path Path to the on-disk file to open
 * @return GIT_SUCCESS on success; error code otherwise
 */
GIT_EXTERN(int) git_config_open_ondisk(git_config **cfg, const char *path);

/**
 * Free the configuration and its associated memory and files
 *
 * @param cfg the configuration to free
 */
GIT_EXTERN(void) git_config_free(git_config *cfg);

/**
 * Get the value of an integer config variable.
 *
 * @param cfg where to look for the variable
 * @param name the variable's name
 * @param out pointer to the variable where the value should be stored
 * @return GIT_SUCCESS on success; error code otherwise
 */
GIT_EXTERN(int) git_config_get_int(git_config *cfg, const char *name, int *out);

/**
 * Get the value of a long integer config variable.
 *
 * @param cfg where to look for the variable
 * @param name the variable's name
 * @param out pointer to the variable where the value should be stored
 * @return GIT_SUCCESS on success; error code otherwise
 */
GIT_EXTERN(int) git_config_get_long(git_config *cfg, const char *name, long int *out);

/**
 * Get the value of a boolean config variable.
 *
 * This function uses the usual C convention of 0 being false and
 * anything else true.
 *
 * @param cfg where to look for the variable
 * @param name the variable's name
 * @param out pointer to the variable where the value should be stored
 * @return GIT_SUCCESS on success; error code otherwise
 */
GIT_EXTERN(int) git_config_get_bool(git_config *cfg, const char *name, int *out);

/**
 * Get the value of a string config variable.
 *
 * The string is owned by the variable and should not be freed by the
 * user.
 *
 * @param cfg where to look for the variable
 * @param name the variable's name
 * @param out pointer to the variable's value
 * @return GIT_SUCCESS on success; error code otherwise
 */
GIT_EXTERN(int) git_config_get_string(git_config *cfg, const char *name, const char **out);

/**
 * Set the value of an integer config variable.
 *
 * @param cfg where to look for the variable
 * @param name the variable's name
 * @param value Integer value for the variable
 * @return GIT_SUCCESS on success; error code otherwise
 */
GIT_EXTERN(int) git_config_set_int(git_config *cfg, const char *name, int value);

/**
 * Set the value of a long integer config variable.
 *
 * @param cfg where to look for the variable
 * @param name the variable's name
 * @param value Long integer value for the variable
 * @return GIT_SUCCESS on success; error code otherwise
 */
GIT_EXTERN(int) git_config_set_long(git_config *cfg, const char *name, long int value);

/**
 * Set the value of a boolean config variable.
 *
 * @param cfg where to look for the variable
 * @param name the variable's name
 * @param value the value to store
 * @return GIT_SUCCESS on success; error code otherwise
 */
GIT_EXTERN(int) git_config_set_bool(git_config *cfg, const char *name, int value);

/**
 * Set the value of a string config variable.
 *
 * A copy of the string is made and the user is free to use it
 * afterwards.
 *
 * @param cfg where to look for the variable
 * @param name the variable's name
 * @param value the string to store.
 * @return GIT_SUCCESS on success; error code otherwise
 */
GIT_EXTERN(int) git_config_set_string(git_config *cfg, const char *name, const char *value);

/**
 * Delete a config variable
 *
 * @param cfg the configuration
 * @param name the variable to delete
 */
GIT_EXTERN(int) git_config_delete(git_config *cfg, const char *name);

/**
 * Perform an operation on each config variable.
 *
 * The callback receives the normalized name and value of each variable
 * in the config backend, and the data pointer passed to this function.
 * As soon as one of the callback functions returns something other than 0,
 * this function returns that value.
 *
 * @param cfg where to get the variables from
 * @param callback the function to call on each variable
 * @param payload the data to pass to the callback
 * @return GIT_SUCCESS or the return value of the callback which didn't return 0
 */
GIT_EXTERN(int) git_config_foreach(
	git_config *cfg,
	int (*callback)(const char *var_name, const char *value, void *payload),
	void *payload);
-}

-------------------------------------------------------------------------------
-- END: config.h
-------------------------------------------------------------------------------



-------------------------------------------------------------------------------
-- BEGIN: errors.h
-------------------------------------------------------------------------------

lastError :: IO String
lastError = peekCString =<< {#call git_lasterror#}

clearError :: IO ()
clearError = {#call git_clearerror#}

-------------------------------------------------------------------------------
-- END: errors.h
-------------------------------------------------------------------------------



-------------------------------------------------------------------------------
-- BEGIN: index.h
-------------------------------------------------------------------------------
{#enum define IdxEntry { GIT_IDXENTRY_NAMEMASK as NameMask
                       , GIT_IDXENTRY_STAGEMASK as StageMask
                       , GIT_IDXENTRY_EXTENDED as ExtendedOrSkipWorkTree
                       , GIT_IDXENTRY_VALID as ValidOrExtended2
                       , GIT_IDXENTRY_STAGESHIFT as StageShift
                       , GIT_IDXENTRY_UPDATE as Update
                       , GIT_IDXENTRY_REMOVE as Remove
                       , GIT_IDXENTRY_UPTODATE as UpToDate
                       , GIT_IDXENTRY_ADDED as Added
                       , GIT_IDXENTRY_HASHED as Hashed
                       , GIT_IDXENTRY_UNHASHED as UnHashed
                       , GIT_IDXENTRY_WT_REMOVE as WTRemove
                       , GIT_IDXENTRY_CONFLICTED as Conflicted
                       , GIT_IDXENTRY_UNPACKED as Unpacked
                       , GIT_IDXENTRY_NEW_SKIP_WORKTREE as NewSkipWorkTree
                       , GIT_IDXENTRY_INTENT_TO_ADD as IntentToAdd
                       , GIT_IDXENTRY_SKIP_WORKTREE as SkipWorkTree
                       , GIT_IDXENTRY_EXTENDED2 as Extended2
                       }#}

-- TODO: Can we get this into IdxEntry somehow?
idxExtFlags :: Int
idxExtFlags = fromEnum IntentToAdd .|. fromEnum SkipWorkTree

-- | Create a new bare Git index object as a memory representation of the Git
-- index file in the provided path, without a repository to back it.
openIndex :: String -> IO (Either GitError Index)
openIndex path = alloca $ \index -> do
  pth <- newCString path
  res <- {#call git_index_open#} index pth
  retEither res $ fmap (Right . Index) $ peek index

-- | Clear the contents (all the entries) of an index object. This clears the
-- index object in memory; changes must be manually written to disk for them to
-- take effect.
clearIndex :: Index -> IO ()
clearIndex (Index idx) = {#call git_index_clear#} idx

-- | Free an existing index object.
freeIndex :: Index -> IO ()
freeIndex (Index idx) = {#call git_index_free#} idx

-- | Update the contents of an existing index object in memory by reading from
-- the hard disk.
readIndex :: Index -> IO (Maybe GitError)
readIndex (Index idx) = do
  res <- {#call git_index_read#} idx
  retMaybeRes res

-- | Write an existing index object from memory back to disk using an atomic
-- file lock.
writeIndex :: Index -> IO (Maybe GitError)
writeIndex (Index idx) = do
  res <- {#call git_index_write#} idx
  retMaybeRes res

-- | Find the first index of any entries which point to given path in the Git
-- index.
findIndex :: Index -> String -> IO (Maybe Int)
findIndex (Index idx) path = do
  pth <- newCString path
  res <- {#call git_index_find#} idx pth
  if res >= 0
    then return . Just $ fromIntegral res
    else return Nothing

-- | Remove all entries with equal path except last added
uniqIndex :: Index -> IO ()
uniqIndex (Index idx) = {#call git_index_uniq#} idx

-- | Add or update an index entry from a file in disk
addIndex :: Index -> String -> Int -> IO (Maybe GitError)
addIndex (Index idx) path stage = do
  pth <- newCString path
  res <- {#call git_index_add#} idx pth (fromIntegral stage)
  retMaybeRes res

-- | Add or update an index entry from an in-memory struct
addIndex2 :: Index -> IndexEntry -> IO (Maybe GitError)
addIndex2 (Index idx) (IndexEntry ie) = do
  res <- {#call git_index_add2#} idx ie
  retMaybeRes res

-- | Add (append) an index entry from a file in disk
appendIndex :: Index -> String -> Int -> IO (Maybe GitError)
appendIndex (Index idx) path stage = do
  pth <- newCString path
  res <- {#call git_index_append#} idx pth (fromIntegral stage)
  retMaybeRes res

-- | Add (append) an index entry from an in-memory struct
appendIndex2 :: Index -> IndexEntry -> IO (Maybe GitError)
appendIndex2 (Index idx) (IndexEntry ie) = do
  res <- {#call git_index_append2#} idx ie
  retMaybeRes res

-- | Remove an entry from the index
remove :: Index -> Int -> IO (Maybe GitError)
remove (Index idx) n = do
  res <- {#call git_index_remove#} idx (fromIntegral n)
  retMaybeRes res

-- | Get a pointer to one of the entries in the index
getIndex :: Index -> Int -> IO (Maybe IndexEntry)
getIndex (Index idx) n = do
  res <- {#call git_index_get#} idx (fromIntegral n)
  return $ if res == nullPtr
             then Nothing
             else Just $ IndexEntry res

-- | Get the count of entries currently in the index
entryCount :: Index -> IO Int
entryCount (Index idx) =
  return . fromIntegral =<< {#call git_index_entrycount#} idx

-- | Get the count of unmerged entries currently in the index
entryCountUnMerged :: Index -> IO Int
entryCountUnMerged (Index idx) =
  return . fromIntegral =<< {#call git_index_entrycount_unmerged#} idx

-- | Get an unmerged entry from the index.
unmergedByPath :: Index -> String -> IO (Maybe IndexEntryUnMerged)
unmergedByPath (Index idx) path = do
  pth <- newCString path
  res <- {#call git_index_get_unmerged_bypath#} idx pth
  return $ if res == nullPtr
             then Nothing
             else Just $ IndexEntryUnMerged res

-- | Get an unmerged entry from the index.
unmergedByIndex :: Index -> Int -> IO (Maybe IndexEntryUnMerged)
unmergedByIndex (Index idx) n = do
  res <- {#call git_index_get_unmerged_byindex#} idx (fromIntegral n)
  return $ if res == nullPtr
             then Nothing
             else Just $ IndexEntryUnMerged res

-- | Return the stage number from a git index entry
entryStage :: IndexEntry -> IO Int
entryStage (IndexEntry ie) =
  return . fromIntegral =<< {#call git_index_entry_stage#} ie

-------------------------------------------------------------------------------
-- END: index.h
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- BEGIN: object.h
-------------------------------------------------------------------------------

-- | Lookup a reference to one of the objects in a repostory.
lookupObj :: Repository -> ObjID -> OType -> IO (Either GitError GitObj)
lookupObj (Repository r) (ObjID oid) oty = alloca $ \obj -> do
  res <- {#call git_object_lookup#} obj r oid (fromIntegral $ fromEnum oty)
  retEither res $ fmap (Right . GitObj) $ peek obj

-- | Lookup a reference to one of the objects in a repostory, given a prefix of
-- its identifier (short id).
-- TODO: Calculate size of ObjID dynamically, rather than passing an int
lookupObjPref :: Repository -> ObjID -> Int -> OType
              -> IO (Either GitError GitObj)
lookupObjPref (Repository r) (ObjID o) n oty = alloca $ \obj -> do
  res <- {#call git_object_lookup_prefix#} obj r o (fromIntegral n)
                                           (fromIntegral $ fromEnum oty)
  retEither res $ fmap (Right . GitObj) $ peek obj

-- | Get the id (SHA1) of a repository object
objId :: GitObj -> ObjID
objId (GitObj go) = unsafePerformIO $
  return . ObjID =<< {#call unsafe git_object_id#} go

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
-------------------------------------------------------------------------------
-- END: object.h
-------------------------------------------------------------------------------



-------------------------------------------------------------------------------
-- BEGIN: repositoy.h
-------------------------------------------------------------------------------

repoIs :: Ptr2Int -> Repository -> IO Bool
repoIs ffi (Repository ptr) = return . toBool =<< ffi ptr

openRepo :: String -> IO (Either GitError Repository)
openRepo path = alloca $ \pprepo -> do
  pstr <- newCString path
  res  <- {#call git_repository_open#} pprepo pstr
  retEither res $ fmap (Right . Repository) $ peek pprepo

openRepoObjDir :: String -> String -> String -> String
               -> IO (Either GitError Repository)
openRepoObjDir dir objDir idxFile workTree = alloca $ \pprepo -> do
  dirStr  <- newCString dir
  objDStr <- newCString objDir
  idxFStr <- newCString idxFile
  wtrStr  <- newCString workTree
  res     <- {#call git_repository_open2#} pprepo dirStr objDStr idxFStr wtrStr
  retEither res $ fmap (Right . Repository) $ peek pprepo

openRepoObjDb :: String -> ObjDB -> String -> String
              -> IO (Either GitError Repository)
openRepoObjDb dir (ObjDB db) idxFile workTree = alloca $ \pprepo -> do
  dirStr  <- newCString dir
  idxFStr <- newCString idxFile
  wtrStr  <- newCString workTree
  res     <- {#call git_repository_open3#} pprepo dirStr db idxFStr wtrStr
  retEither res $ fmap (Right . Repository) $ peek pprepo

discover :: String -> Bool -> String -> IO (Either GitError String)
discover startPath acrossFs ceilingDirs = alloca $ \path -> do
  spStr  <- newCString startPath
  cdsStr <- newCString ceilingDirs
  res    <- {#call git_repository_discover#} path (fromIntegral $ sizeOf path)
                                             spStr (fromBool acrossFs) cdsStr
  retEither res $ fmap Right $ peekCString path

database :: Repository -> IO ObjDB
database (Repository r) = return . ObjDB =<< {#call git_repository_database#} r

index :: Repository -> IO (Either GitError Index)
index (Repository r) = alloca $ \idx -> do
  res  <- {#call git_repository_index#} idx r
  retEither res $ fmap (Right . Index) $ peek idx

free :: Repository -> IO ()
free (Repository r) = {#call git_repository_free#} r

init :: String -> Bool -> IO (Either GitError Repository)
init path isBare = alloca $ \pprepo -> do
  pstr <- newCString path
  res  <- {#call git_repository_init#} pprepo pstr (fromBool isBare)
  retEither res $ fmap (Right . Repository) $ peek pprepo

isHeadDetached :: Repository -> IO Bool
isHeadDetached = repoIs {#call git_repository_head_detached#}

isHeadOrphan :: Repository -> IO Bool
isHeadOrphan = repoIs {#call git_repository_head_orphan#}

isEmpty :: Repository -> IO Bool
isEmpty = repoIs {#call git_repository_is_empty#}

path :: Repository -> RepositoryPathID -> IO String
path (Repository r) pathID = peekCString =<< {#call git_repository_path#} r p
  where p = fromIntegral $ fromEnum pathID

isBare :: Repository -> IO Bool
isBare = repoIs {#call git_repository_is_bare#}

-------------------------------------------------------------------------------
-- END: repository.h
-------------------------------------------------------------------------------

