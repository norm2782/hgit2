{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}

#include <git2/odb_backend.h>

module Data.HGit2.ODBBackend where

import Data.HGit2.Git2
import Foreign
import Foreign.C

newtype ODBBackend = ODBBackend CPtr

instance CWrapper ODBBackend where
  unwrap (ODBBackend o) = o

{#enum git_odb_streammode as ODBStreamMode {underscoreToCase}#}

packBackend :: String -> IOEitherErr ODBBackend
packBackend = backend {#call git_odb_backend_pack#}

looseBackend :: String -> IOEitherErr ODBBackend
looseBackend = backend {#call git_odb_backend_loose#}

backend :: (Ptr (Ptr ()) -> CString -> IO CInt) -> String
        -> IOEitherErr ODBBackend
backend fn str = withCString str $ \str' ->
  callPeek ODBBackend (\out -> fn out str')
