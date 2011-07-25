{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}

#include <git2/remote.h>

module Data.HGit2.Remote where

import Data.HGit2.Git2
import Data.HGit2.Config
import Data.HGit2.Types
import Foreign
import Foreign.C

newtype Remote = Remote CPtr
newtype HeadArray = HeadArray CPtr

instance CWrapper Remote where
  unwrap (Remote r) = r

instance CWrapper HeadArray where
  unwrap (HeadArray a) = a

-- | Get the information for a particular remote
remote :: Config -> String -> IOEitherErr Remote
remote (Config cfp) str =
  withForeignPtr cfp $ \c ->
  withCString str $ \str' ->
  callPeek Remote (\out -> {#call git_remote_get#} out c str')

-- | Get the remote's name
remoteName :: Remote -> String
remoteName (Remote rfp) = unsafePerformIO $
  withForeignPtr rfp $ \r ->
  peekCString =<< {#call git_remote_name#} r

-- | Get the remote's url
remoteURL :: Remote -> String
remoteURL (Remote rfp) = unsafePerformIO $
  withForeignPtr rfp $ \r ->
  peekCString =<< {#call git_remote_url#} r

-- | Get the fetch refspec
remoteRefSpec :: Remote -> IO (Maybe RefSpec)
remoteRefSpec (Remote rfp) =
  withForeignPtr rfp $ \r -> do
  ptr <- mkFPtr =<< {#call git_remote_fetchspec#} r
  retRes RefSpec ptr

-- | Get the push refspec
pushSpec :: Remote -> IO (Maybe RefSpec)
pushSpec (Remote rfp) =
  withForeignPtr rfp $ \r -> do
  ptr <- mkFPtr =<< {#call git_remote_pushspec#} r
  retRes RefSpec ptr

-- | Open a connection to a remote
--
-- The transport is selected based on the URL
connect :: Remote -> Int -> IOCanFail
connect (Remote rfp) n =
  withForeignPtr rfp $ \r ->
  retMaybe =<< {#call git_remote_connect#} r (fromIntegral n)

-- | Get a list of refs at the remote
--
-- The remote (or more exactly its transport) must be connected.
remoteLs :: Remote -> HeadArray -> IOCanFail
remoteLs (Remote rfp) (HeadArray hfp) =
  withForeignPtr rfp $ \r ->
  withForeignPtr hfp $ \h ->
  retMaybe =<< {#call git_remote_ls#} r h
