{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}

#include <git2/reflog.h>

module Data.HGit2.Reflog where

import Data.HGit2.Git2
import Data.HGit2.Signature
import Data.HGit2.OID
import Foreign
import Foreign.C


newtype Reference = Reference CPtr
newtype Reflog = Reflog CPtr
newtype ReflogEntry = ReflogEntry CPtr

instance CWrapper Reference where
  unwrap (Reference r) = r

instance CWrapper Reflog where
  unwrap (Reflog r) = r

instance CWrapper ReflogEntry where
  unwrap (ReflogEntry r) = r

-- | Read the reflog for the given reference
--
-- The reflog must be freed manually by using freeReflog
readReflog :: Reference -> IOEitherErr Reflog
readReflog (Reference rfp) =
  withForeignPtr rfp $ \r ->
  callPeek Reflog (\out -> {#call git_reflog_read#} out r)

-- | Write a new reflog for the given reference
--
-- If there is no reflog file for the given reference yet, it will be created.
writeReflog :: Reference -> OID -> Signature -> String -> IOCanFail
writeReflog (Reference rfp) (OID ofp) (Signature sfp) str =
  withForeignPtr rfp $ \r ->
  withForeignPtr ofp $ \o ->
  withForeignPtr sfp $ \s ->
  withCString str $ \str' ->
  retMaybe =<< {#call git_reflog_write#} r o s str'

-- | Get the number of log entries in a reflog
entryCount :: Reflog -> IO Int
entryCount = wrpToInt {#call git_reflog_entrycount#}

-- | Lookup an entry by its index
entryByIndex :: Reflog -> Int -> IO (Maybe ReflogEntry)
entryByIndex (Reflog rfp) n =
  withForeignPtr rfp $ \r -> do
  ptr <- mkFPtr =<< {#call git_reflog_entry_byindex#} r (fromIntegral n)
  retRes ReflogEntry ptr

-- | Get the old oid
oldOID :: ReflogEntry -> String
oldOID = unsafePerformIO . wrpToStr {#call unsafe git_reflog_entry_oidold#}

-- | Get the new oid
newOID :: ReflogEntry -> String
newOID = unsafePerformIO . wrpToStr {#call unsafe git_reflog_entry_oidnew#}

-- | Get the committer of this entry
committer :: ReflogEntry -> Signature
committer = unsafePerformIO . wrpToCstr Signature
  {#call unsafe git_reflog_entry_committer#}

-- | Get the log msg
entryMsg :: ReflogEntry -> String
entryMsg = unsafePerformIO . wrpToStr {#call unsafe git_reflog_entry_msg#}
