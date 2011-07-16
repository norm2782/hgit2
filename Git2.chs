{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE StandaloneDeriving #-}

#include "git2.h"

module Git2 where

import Foreign
import Foreign.C.String
import Foreign.C.Types

type OffT    = {#type git_off_t#}
type TimeT   = {#type git_time_t#}
type CPtr    = Ptr ()
type Ptr2Int = CPtr -> IO CInt

newtype ObjDB      = ObjDB CPtr
newtype Repository = Repository CPtr
newtype Index      = Index CPtr
newtype Blob       = Blob CPtr
newtype ObjID      = ObjID CPtr

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

-------------------------------------------------------------------------------
-- BEGIN: blob.h
-------------------------------------------------------------------------------


{-
/**
 * Lookup a blob object from a repository.
 *
 * @param blob pointer to the looked up blob
 * @param repo the repo to use when locating the blob.
 * @param id identity of the blob to locate.
 * @return 0 on success; error code otherwise
 */
GIT_INLINE(int) git_blob_lookup(git_blob **blob, git_repository *repo, const git_oid *id)
-}
blobLookup :: Repository -> ObjID -> IO (Either GitError Blob)
blobLookup = undefined


{-

/**
 * Lookup a blob object from a repository,
 * given a prefix of its identifier (short id).
 *
 * @see git_object_lookup_prefix
 *
 * @param blob pointer to the looked up blob
 * @param repo the repo to use when locating the blob.
 * @param id identity of the blob to locate.
 * @param len the length of the short identifier
 * @return 0 on success; error code otherwise
 */
GIT_INLINE(int) git_blob_lookup_prefix(git_blob **blob, git_repository *repo, const git_oid *id, unsigned int len)
{
	return git_object_lookup_prefix((git_object **)blob, repo, id, len, GIT_OBJ_BLOB);
}
-}
blobLookupPrefix :: Repository -> ObjID -> Int -> IO (Either GitError Blob)
blobLookupPrefix = undefined


{-
/**
 * Close an open blob
 *
 * This is a wrapper around git_object_close()
 *
 * IMPORTANT:
 * It *is* necessary to call this method when you stop
 * using a blob. Failure to do so will cause a memory leak.
 *
 * @param blob the blob to close
 */

GIT_INLINE(void) git_blob_close(git_blob *blob)
{
	git_object_close((git_object *) blob);
}
-}
closeBlob :: Blob -> IO ()
closeBlob = undefined


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
-- Return raw bytestring ? :x
rawBlobContent = undefined


{-
/**
 * Get the size in bytes of the contents of a blob
 *
 * @param blob pointer to the blob
 * @return size on bytes
 */
GIT_EXTERN(int) git_blob_rawsize(git_blob *blob);
-}
rawBlobSize :: Blob -> Int
rawBlobSize = undefined

{-
/**
 * Read a file from the working folder of a repository
 * and write it to the Object Database as a loose blob
 *
 * @param oid return the id of the written blob
 * @param repo repository where the blob will be written.
 *	this repository cannot be bare
 * @param path file from which the blob will be created,
 *	relative to the repository's working dir
 * @return 0 on success; error code otherwise
 */
GIT_EXTERN(int) git_blob_create_fromfile(git_oid *oid, git_repository *repo, const char *path);
-}
createBlobFromFile :: ObjID -> Repository -> String -> IO (Maybe GitError)
createBlobFromFile (ObjID o) (Repository r) path = do
  pathStr  <- newCString path
  res      <- {#call git_blob_create_fromfile #} o r pathStr
  if res == 0
    then return Nothing
    else return $ Just . toEnum . fromIntegral $ res

{-
/**
 * Write an in-memory buffer to the ODB as a blob
 *
 * @param oid return the oid of the written blob
 * @param repo repository where to blob will be written
 * @param buffer data to be written into the blob
 * @param len length of the data
 * @return 0 on success; error code otherwise
 */
GIT_EXTERN(int) git_blob_create_frombuffer(git_oid *oid, git_repository *repo, const void *buffer, size_t len);
-}
-- createBlobFromBuffer :: ObjID -> Repository -> ... -> Int -> IO (Maybe GitError)
createBlobFromBuffer = undefined


-------------------------------------------------------------------------------
-- END: blob.h
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

openRepoObjDir :: String -> String -> String -> String -> IO (Either GitError Repository)
openRepoObjDir dir objDir idxFile workTree = alloca $ \pprepo -> do
  dirStr     <- newCString dir
  objDirStr  <- newCString objDir
  idxFileStr <- newCString idxFile
  wtreeStr   <- newCString workTree
  res        <- {#call git_repository_open2#} pprepo dirStr objDirStr idxFileStr wtreeStr
  retEither res $ fmap (Right . Repository) $ peek pprepo

openRepoObjDb :: String -> ObjDB -> String -> String -> IO (Either GitError Repository)
openRepoObjDb dir (ObjDB db) idxFile workTree = alloca $ \pprepo -> do
  dirStr     <- newCString dir
  idxFileStr <- newCString idxFile
  wtreeStr   <- newCString workTree
  res        <- {#call git_repository_open3#} pprepo dirStr db idxFileStr wtreeStr
  retEither res $ fmap (Right . Repository) $ peek pprepo

discover :: String -> Bool -> String -> IO (Either GitError String)
discover startPath acrossFs ceilingDirs = alloca $ \path -> do
  spStr  <- newCString startPath
  cdsStr <- newCString ceilingDirs
  res    <- {#call git_repository_discover#} path (fromIntegral $ sizeOf path) spStr (fromBool acrossFs) cdsStr
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

retEither :: CInt -> IO (Either GitError a) -> IO (Either GitError a)
retEither res f | res == 0  = f
                | otherwise = return . Left . toEnum . fromIntegral $ res

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

