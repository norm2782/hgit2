{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}

#include <git2/transport.h>

module Data.HGit2.Transport where

import Data.HGit2.Git2
import Data.HGit2.Remote
import Foreign
import Foreign.C

newtype Transport = Transport CPtr

instance CWrapper Transport where
  unwrap (Transport t) = t

-- | Get the appropriate transport for an URL.
new :: String -> IOEitherErr Transport
new u =
  withCString u $ \url ->
  callPeek Transport (\out -> {#call git_transport_new#} out url)

connect :: Transport -> Int -> IOCanFail
connect (Transport tfp) n =
  withForeignPtr tfp $ \t ->
  retMaybe =<< {#call git_transport_connect#} t (fromIntegral n)

ls :: Transport -> HeadArray -> IOCanFail
ls (Transport tfp) (HeadArray hfp) =
  withForeignPtr tfp $ \t ->
  withForeignPtr hfp $ \h ->
  retMaybe =<< {#call git_transport_ls#} t h

close :: Transport -> IOCanFail
close (Transport tfp) =
  withForeignPtr tfp $ \t ->
  retMaybe =<< {#call git_transport_close#} t

{- add :: Transport -> String -> IOCanFail-}
{- add (Transport t) st =-}
  {- retMaybe =<< {#call git_transport_add#} t =<< newCString st-}
-- FIXME: Linking problems?
