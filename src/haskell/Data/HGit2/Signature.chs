{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}

#include <git2/signature.h>

module Data.HGit2.Signature where

import Data.HGit2.Git2
import Data.HGit2.Types
import Foreign
import Foreign.C

newtype Signature = Signature CPtr

instance CWrapper Signature where
  unwrap (Signature s) = s

-- | Create a new action signature.
newSignature :: String -> String -> TimeT -> Int -> IO (Maybe Signature)
newSignature nm em t off =
  withCString nm $ \nm' ->
  withCString em $ \em' ->
  retSig =<< mkFPtr =<< {#call git_signature_new#} nm' em' t (fromIntegral off)

-- | Create a new action signature with a timestamp of 'now'.
nowSignature :: String -> String -> IO (Maybe Signature)
nowSignature nm em =
  withCString nm $ \nm' ->
  withCString em $ \em' ->
  retSig =<< mkFPtr =<< {#call git_signature_now#} nm' em'

retSig :: CPtr -> IO (Maybe Signature)
retSig = retRes Signature

-- | Create a copy of an existing signature.
dupSignature :: Signature -> IO (Maybe Signature)
dupSignature (Signature sfp) =
  withForeignPtr sfp $ \sig ->
  retSig =<< mkFPtr =<< {#call git_signature_dup#} sig
