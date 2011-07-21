{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}

#include <git2/common.h>

module Data.HGit2.Common where

import Data.HGit2.Git2

newtype StrArray = StrArray CPtr

instance CWrapper StrArray where
  unwrap (StrArray a) = a

