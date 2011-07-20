{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}

#include <git2/odb_backend.h>

module Data.HGit2.ODBBackend where

import Data.HGit2.Git2
import Data.HGit2.Types
import Data.HGit2.OID
import Foreign
import Foreign.C

newtype ODBBackend = ODBBackend CPtr

{#enum git_odb_streammode as ODBStreamMode {underscoreToCase}#}

packBackend :: String -> IOEitherErr ODBBackend
packBackend = backend {#call git_odb_backend_pack#}

looseBackend :: String -> IOEitherErr ODBBackend
looseBackend = backend {#call git_odb_backend_loose#}

backend :: (Ptr CPtr -> CString -> IO CInt) -> String -> IOEitherErr ODBBackend
backend fn str =
  alloca $ \out -> eitherPeek out ODBBackend =<< fn out =<< newCString str