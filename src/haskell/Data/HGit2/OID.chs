{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}

#include <git2/oid.h>

module Data.HGit2.OID where

import Data.HGit2.Git2
import Foreign
import Foreign.C

newtype OID = OID CPtr

instance CWrapper OID where
  unwrap (OID o) = o

{#enum define OidSizes { GIT_OID_RAWSZ as RawSize
                       , GIT_OID_HEXSZ as HexSize
                       , GIT_OID_MINPREFIXLEN as MinPrefixLen}#}


{-
/**
 * Parse a hex formatted object id into a git_oid.
 *
 * @param out oid structure the result is written into.
 * @param str input hex string; must be pointing at the start of
 *        the hex sequence and have at least the number of bytes
 *        needed for an oid encoded in hex (40 bytes).
 * @return GIT_SUCCESS if valid; GIT_ENOTOID on failure.
 */
GIT_EXTERN(int) git_oid_fromstr(git_oid *out, const char *str);
TODO
-}
{- oidFromStr :: String -> IO (Either GitError OID)-}
{- oidFromStr str = alloca $ \oid -> do-}
  {- s' <- newCString str-}
  {- res <- {#call git_oid_fromstr#} oid s'-}
  {- retEither res $ return $ (Right . OID) oid-}

{-
/**
 * Parse N characters of a hex formatted object id into a git_oid
 *
 * If N is odd, N-1 characters will be parsed instead.
 * The remaining space in the git_oid will be set to zero.
 *
 * @param out oid structure the result is written into.
 * @param str input hex string of at least size `length`
 * @param length length of the input string
 * @return GIT_SUCCESS if valid; GIT_ENOTOID on failure.
 */
GIT_EXTERN(int) git_oid_fromstrn(git_oid *out, const char *str, size_t length);

/**
 * Copy an already raw oid into a git_oid structure.
 *
 * @param out oid structure the result is written into.
 * @param raw the raw input bytes to be copied.
 */
GIT_EXTERN(void) git_oid_fromraw(git_oid *out, const unsigned char *raw);
-}

-- | Format a git_oid into a hex string.
fmtOID :: OID -> IO String
fmtOID (OID ofp) = withForeignPtr ofp $ \o ->
  alloca $ \out -> do
  {#call git_oid_fmt#} out o
  peekCString out

-- | Format a git_oid into a loose-object path string.
--
-- The resulting string is "aa/...", where "aa" is the first two hex digitis of
-- the oid and "..." is the remaining 38 digits.
pathFmt :: OID -> IO String
pathFmt (OID ofp) = withForeignPtr ofp $ \o ->
  alloca $ \out -> do
  {#call git_oid_pathfmt#} out o
  peekCString out

-- | Format an OID into a newly allocated String.
formatOid :: OID -> IO (Maybe String)
formatOid (OID ofp) =
  withForeignPtr ofp $ \o -> do
  res <- {#call git_oid_allocfmt#} o
  if res == nullPtr
    then return Nothing
    else fmap Just $ peekCString res

{-
/**
 * Format a git_oid into a buffer as a hex format c-string.
 *
 * If the buffer is smaller than GIT_OID_HEXSZ+1, then the resulting
 * oid c-string will be truncated to n-1 characters. If there are
 * any input parameter errors (out == NULL, n == 0, oid == NULL),
 * then a pointer to an empty string is returned, so that the return
 * value can always be printed.
 *
 * @param out the buffer into which the oid string is output.
 * @param n the size of the out buffer.
 * @param oid the oid structure to format.
 * @return the out buffer pointer, assuming no input parameter
 *         errors, otherwise a pointer to an empty string.
 */
GIT_EXTERN(char *) git_oid_to_string(char *out, size_t n, const git_oid *oid);
-}

-- | Copy an oid from one structure to another.
copyOID :: OID -> OID -> IO ()
copyOID (OID afp) (OID bfp) =
  withForeignPtr afp $ \a ->
  withForeignPtr bfp $ \b ->
  {#call git_oid_cpy#} a b

-- | Compare two oid structures.
cmpOID :: OID -> OID -> Int
cmpOID (OID afp) (OID bfp) = unsafePerformIO $
  withForeignPtr afp $ \a ->
  withForeignPtr bfp $ \b ->
  return . fromIntegral =<< {#call git_oid_cmp#} a b

instance Ord OID where
  compare a b | c <  0    = LT
              | c == 0    = EQ
              | otherwise = GT
              where c = cmpOID a b

instance Eq OID where
  a == b = compare a b == EQ

-- | Compare the first 'len' hexadecimal characters (packets of 4 bits) of two
-- oid structures.
ncmp :: OID -> OID -> Int -> Bool
ncmp (OID afp) (OID bfp) n =
  unsafePerformIO $
  withForeignPtr afp $ \a ->
  withForeignPtr bfp $ \b -> do
  res <- {#call unsafe git_oid_ncmp#} a b (fromIntegral n)
  return $ res == 0
