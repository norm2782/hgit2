{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE CPP #-}

#include <git2.h>

module Data.HGit2.Git2 where

import Data.Bits
import Data.Maybe
import Foreign
import Foreign.C.String
import Foreign.C.Types
import System.IO.Unsafe

type CPtr    = Ptr ()
type Ptr2Int = CPtr -> IO CInt
