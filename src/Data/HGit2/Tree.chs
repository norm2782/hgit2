{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}

#include <git2/tree.h>

module Data.HGit2.Tree where

import Data.Bits
import Data.HGit2.Git2
import Data.Maybe
import Foreign
import Foreign.C.String
import Foreign.C.Types

newtype Tree = Tree CPtr
