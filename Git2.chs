{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE StandaloneDeriving #-}

#include "git2.h"

module Git2 where

import Data.ByteString
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

{-
/**
 * Get the id of the tree pointed to by a commit. This differs from
 * `git_commit_tree` in that no attempts are made to fetch an object
 * from the ODB.
 *
 * @param commit a previously loaded commit.
 * @return the id of tree pointed to by commit.
 */
GIT_EXTERN(const git_oid *) git_commit_tree_oid(git_commit *commit);

/**
 * Get the number of parents of this commit
 *
 * @param commit a previously loaded commit.
 * @return integer of count of parents
 */
GIT_EXTERN(unsigned int) git_commit_parentcount(git_commit *commit);
-}

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

{-
/**
 * Create a new commit in the repository using `git_object`
 * instances as parameters.
 *
 * @param oid Pointer where to store the OID of the
 *	newly created commit
 *
 * @param repo Repository where to store the commit
 *
 * @param update_ref If not NULL, name of the reference that
 *	will be updated to point to this commit. If the reference
 *	is not direct, it will be resolved to a direct reference.
 *	Use "HEAD" to update the HEAD of the current branch and
 *	make it point to this commit
 *
 * @param author Signature representing the author and the authory
 *	time of this commit
 *
 * @param committer Signature representing the committer and the
 *  commit time of this commit
 *
 * @param message Full message for this commit
 *
 * @param tree An instance of a `git_tree` object that will
 * be used as the tree for the commit. This tree object must
 * also be owned by the given `repo`.
 *
 * @param parent_count Number of parents for this commit
 *
 * @param parents[] Array of `parent_count` pointers to `git_commit`
 * objects that will be used as the parents for this commit. This
 * array may be NULL if `parent_count` is 0 (root commit). All the
 * given commits must be owned by the `repo`.
 *
 * @return 0 on success; error code otherwise
 *	The created commit will be written to the Object Database and
 *	the given reference will be updated to point to it
 */
GIT_EXTERN(int) git_commit_create(
		git_oid *oid,
		git_repository *repo,
		const char *update_ref,
		const git_signature *author,
		const git_signature *committer,
		const char *message,
		const git_tree *tree,
		int parent_count,
		const git_commit *parents[]);

/**
 * Create a new commit in the repository using a variable
 * argument list.
 *
 * The parents for the commit are specified as a variable
 * list of pointers to `const git_commit *`. Note that this
 * is a convenience method which may not be safe to export
 * for certain languages or compilers
 *
 * All other parameters remain the same
 *
 * @see git_commit_create
 */
GIT_EXTERN(int) git_commit_create_v(
		git_oid *oid,
		git_repository *repo,
		const char *update_ref,
		const git_signature *author,
		const git_signature *committer,
		const char *message,
		const git_tree *tree,
		int parent_count,
		...);
-}

-------------------------------------------------------------------------------
-- END: commit.h
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

