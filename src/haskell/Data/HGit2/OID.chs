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

-- | Parse a hex formatted object id into a git_oid.
oidFromStr :: OID -> String -> IOCanFail
oidFromStr (OID ofp) str =
  withForeignPtr ofp $ \o ->
  withCString str $ \s' ->
  retMaybe =<< {#call git_oid_fromstr#} o s'

-- | Parse a hex formatted object id into a git_oid
fromStr :: OID -> String -> IOCanFail
fromStr (OID ofp) str =
  withForeignPtr ofp $ \o ->
  withCStringLen str $ \(xs, ln) ->
  retMaybe =<< {#call git_oid_fromstrn#} o xs (fromIntegral ln)

-- | Copy an already raw oid into a git_oid structure.
fromRaw :: OID -> String -> IO ()
fromRaw (OID ofp) str =
  withForeignPtr ofp $ \o ->
  withCUString str $ \r ->
  {#call git_oid_fromraw#} o r

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

-- | Format a git_oid into a buffer as a hex format c-string.
oidToString :: OID -> IO String
oidToString (OID ofp) =
  withForeignPtr ofp $ \o -> do
  str <- malloc
  res <- peekCString =<< {#call git_oid_to_string#} str (fromIntegral $ sizeOf str) o
  free str
  return res

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
