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

-- | Create a new action signature. The signature must be freed manually or
-- using freeSignature
newSignature :: String -> String -> TimeT -> Int -> IO (Maybe Signature)
newSignature nm em t off = do
  nm' <- newCString nm
  em' <- newCString em
  retRes =<< {#call git_signature_new#} nm' em' t (fromIntegral off)

-- | Create a new action signature with a timestamp of 'now'. The signature
-- must be freed manually or using freeSignature
nowSignature :: String -> String -> IO (Maybe Signature)
nowSignature nm em = do
  nm' <- newCString nm
  em' <- newCString em
  retRes =<< {#call git_signature_now#} nm' em'

retRes :: CPtr -> IO (Maybe Signature)
retRes = return . retRes'
  where retRes' res | res == nullPtr = Nothing
                    | otherwise      = Just $ Signature res

-- | Create a copy of an existing signature.
dupSignature :: Signature -> IO (Maybe Signature)
dupSignature = (retRes =<<) . {#call git_signature_dup#} . unwrap

-- | Free an existing signature
freeSignature :: Signature -> IO ()
freeSignature = {#call git_signature_free#} . unwrap
