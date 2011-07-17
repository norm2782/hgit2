{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}

#include <git2/signature.h>

module Data.HGit2.Signature where

import Data.HGit2.Git2

newtype Signature = Signature CPtr

