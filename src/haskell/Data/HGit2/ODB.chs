{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}

#include <git2/odb.h>

module Data.HGit2.ODB where

import Data.HGit2.Git2
import Data.HGit2.Types
import Data.HGit2.ODBBackend
import Data.HGit2.OID
import Foreign
import Foreign.C

newtype ODB = ODB CPtr
newtype ODBObj = ODBObj CPtr
newtype ODBStream = ODBStream CPtr

instance CWrapper ODB where
  unwrap (ODB o) = o

instance CWrapper ODBObj where
  unwrap (ODBObj o) = o

-- | Create a new object database with no backends.
--
-- Before the ODB can be used for read/writing, a custom database backend must
-- be manually added using `git_odb_add_backend()`
newODB :: IOEitherErr ODB
newODB = callPeek ODB {#call git_odb_new#}

-- Create a new object database and automatically add the two default backends:
--
-- - git_odb_backend_loose: read and write loose object files from disk,
--   assuming `objects_dir` as the Objects folder
--
-- - git_odb_backend_pack: read objects from packfiles, assuming `objects_dir`
--   as the Objects folder which contains a 'pack/' folder with the
--   corresponding data
openODB :: String -> IOEitherErr ODB
openODB str = callPeek ODB
  (\out -> {#call git_odb_open#} out =<< newCString str)

-- | Add a custom backend to an existing Object DB
--
-- The backends are checked in relative ordering, based on the value of the
-- `priority` parameter.
addODBBackend :: ODB -> ODBBackend -> Int -> IOCanFail
addODBBackend (ODB o) (ODBBackend ob) n =
  retMaybe =<< {#call git_odb_add_backend#} o ob (fromIntegral n)

-- | Add a custom backend to an existing Object DB; this backend will work as
-- an alternate.
--
-- Alternate backends are always checked for objects *after* all the main
-- backends have been exhausted.
--
-- The backends are checked in relative ordering, based on the value of the
-- `priority` parameter.
--
-- Writing is disabled on alternate backends.
addAlternate :: ODB -> ODBBackend -> Int -> IOCanFail
addAlternate (ODB o) (ODBBackend ob) n =
  retMaybe =<< {#call git_odb_add_alternate#} o ob (fromIntegral n)

-- | Close an open object database.
closeODB :: ODB -> IO ()
closeODB = {#call git_odb_close#} . unwrap

-- | Read an object from the database.
--
-- This method queries all available ODB backends trying to read the given OID.
--
-- The returned object is reference counted and internally cached, so it should
-- be closed by the user once it's no longer in use.
readODB :: ODB -> OID -> IOEitherErr ODBObj
readODB (ODB o) (OID i) = callPeek ODBObj
  (\out -> {#call git_odb_read#} out o i)

-- | Read an object from the database, given a prefix of its identifier.
--
-- This method queries all available ODB backends trying to match the 'len'
-- first hexadecimal characters of the 'short_id'. The remaining
-- (GIT_OID_HEXSZ-len)*4 bits of 'short_id' must be 0s. 'len' must be at least
-- GIT_OID_MINPREFIXLEN, and the prefix must be long enough to identify a
-- unique object in all the backends; the method will fail otherwise.
--
-- The returned object is reference counted and internally cached, so it should
-- be closed by the user once it's no longer in use.
readPrefix :: ODB -> OID -> Int -> IOEitherErr ODBObj
readPrefix (ODB o) (OID i) n = callPeek ODBObj
  (\out -> {#call git_odb_read_prefix#} out o i (fromIntegral n))

{-
/**
 * Read the header of an object from the database, without
 * reading its full contents.
 *
 * The header includes the length and the type of an object.
 *
 * Note that most backends do not support reading only the header
 * of an object, so the whole object will be read and then the
 * header will be returned.
 *
 * @param len_p pointer where to store the length
 * @param type_p pointer where to store the type
 * @param db database to search for the object in.
 * @param id identity of the object to read.
 * @return
 * - GIT_SUCCESS if the object was read;
 * - GIT_ENOTFOUND if the object is not in the database.
 */
GIT_EXTERN(int) git_odb_read_header(size_t *len_p, git_otype *type_p, git_odb *db, const git_oid *id);
TODO
-}
readHeader = undefined

-- | Determine if the given object can be found in the object database.
exists :: ODB -> OID -> IO Bool
exists (ODB o) (OID d) = return . toBool =<< {#call git_odb_exists#} o d

{-
/**
 * Write an object directly into the ODB
 *
 * This method writes a full object straight into the ODB.
 * For most cases, it is preferred to write objects through a write
 * stream, which is both faster and less memory intensive, specially
 * for big objects.
 *
 * This method is provided for compatibility with custom backends
 * which are not able to support streaming writes
 *
 * @param oid pointer to store the OID result of the write
 * @param odb object database where to store the object
 * @param data buffer with the data to storr
 * @param len size of the buffer
 * @param type type of the data to store
 * @return 0 on success; error code otherwise
 */
GIT_EXTERN(int) git_odb_write(git_oid *oid, git_odb *odb, const void *data, size_t len, git_otype type);

TODO
-}
{- writeODB :: OID -> ODB -> -}

{-
/**
 * Open a stream to write an object into the ODB
 *
 * The type and final length of the object must be specified
 * when opening the stream.
 *
 * The returned stream will be of type `GIT_STREAM_WRONLY` and
 * will have the following methods:
 *
 *		- stream->write: write `n` bytes into the stream
 *		- stream->finalize_write: close the stream and store the object in
 *			the odb
 *		- stream->free: free the stream
 *
 * The streaming write won't be effective until `stream->finalize_write`
 * is called and returns without an error
 *
 * The stream must always be free'd or will leak memory.
 *
 * @see git_odb_stream
 *
 * @param stream pointer where to store the stream
 * @param db object database where the stream will write
 * @param size final size of the object that will be written
 * @param type type of the object that will be written
 * @return 0 if the stream was created; error code otherwise
 */
GIT_EXTERN(int) git_odb_open_wstream(git_odb_stream **stream, git_odb *db, size_t size, git_otype type);
TODO: Finish this
-}
openWStream :: ODB -> OType -> IOEitherErr ODBStream
openWStream (ODB o) oty = callPeek ODBStream
  (\out -> {#call git_odb_open_wstream#} out o undefined (fromIntegral $ fromEnum oty))

-- | Open a stream to read an object from the ODB
--
-- Note that most backends do *not* support streaming reads because they store
-- their objects as compressed/delta'ed blobs.
--
-- It's recommended to use `git_odb_read` instead, which is assured to work on
-- all backends.
--
-- The returned stream will be of type `GIT_STREAM_RDONLY` and will have the
-- following methods:
--
-- - stream->read: read `n` bytes from the stream
-- - stream->free: free the stream
--
-- The stream must always be free'd or will leak memory.
openRStream :: ODB -> OID -> IOEitherErr ODBStream
openRStream (ODB o) (OID i) = callPeek ODBStream
  (\out -> {#call git_odb_open_rstream#} out o i)

{-
/**
 * Determine the object-ID (sha1 hash) of a data buffer
 *
 * The resulting SHA-1 OID will the itentifier for the data
 * buffer as if the data buffer it were to written to the ODB.
 *
 * @param id the resulting object-ID.
 * @param data data to hash
 * @param len size of the data
 * @param type of the data to hash
 * @return 0 on success; error code otherwise
 */
GIT_EXTERN(int) git_odb_hash(git_oid *id, const void *data, size_t len, git_otype type);

/**
 * Read a file from disk and fill a git_oid with the object id
 * that the file would have if it were written to the Object
 * Database as an object of the given type. Similar functionality
 * to git.git's `git hash-object` without the `-w` flag.
 *
 * @param out oid structure the result is written into.
 * @param path file to read and determine object id for
 * @param type the type of the object that will be hashed
 * @return GIT_SUCCESS if valid; error code otherwise
 */
GIT_EXTERN(int) git_odb_hashfile(git_oid *out, const char *path, git_otype type);
-}

{- hashFile :: String -> OType -> IOEitherErr OID-}
{- hashFile str oty = alloca $ \out -> do-}
  {- str' <- newCString str-}
  {- res <- {#call git_odb_hashfile#} out str' (fromIntegral $ fromEnum oty)-}
  {- retEither res $ fmap (Right . OID) $ peek out-}

-- | Close an ODB object
--
-- This method must always be called once a `git_odb_object` is no longer
-- needed, otherwise memory will leak.
closeODBObj :: ODBObj -> IO ()
closeODBObj = {#call git_odb_object_close#} . unwrap

-- | Return the OID of an ODB object
--
-- This is the OID from which the object was read from
objId :: ODBObj -> IO OID
objId = callRetCons {#call git_odb_object_id#} OID

-- | Return the data of an ODB object
--
-- This is the uncompressed, raw data as read from the ODB, without the leading
-- header.
--
-- This pointer is owned by the object and shall not be free'd.
objData :: ODBObj -> IO RawData
objData = callRetCons {#call git_odb_object_data#} RawData

-- | Return the size of an ODB object
-- This is the real size of the `data` buffer, not the actual size of the
-- object.
odbObjSize :: ODBObj -> IO Integer
odbObjSize = retNum . {#call git_odb_object_size#} . unwrap

-- | Return the type of an ODB object
odbObjType :: ODBObj -> IO OType
odbObjType = retEnum . {#call git_odb_object_type#} . unwrap
