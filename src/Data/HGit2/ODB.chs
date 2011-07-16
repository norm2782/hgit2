{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}

#include <git2/odb.h>

module Data.HGit2.ODB where

import Data.Bits
import Data.HGit2.Git2
import Data.Maybe
import Foreign
import Foreign.C.String
import Foreign.C.Types

newtype ODB = ODB CPtr

{#enum git_odb_streammode as ODBStreamMode {underscoreToCase}#}

