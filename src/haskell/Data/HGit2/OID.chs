{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}

#include <git2/oid.h>

module Data.HGit2.OID where

import Data.HGit2.Git2

newtype OID = OID CPtr

