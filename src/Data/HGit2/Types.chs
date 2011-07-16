{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}

#include <git2.h>

module Data.HGit2.Types where

import Data.Bits
import Data.Maybe
import Foreign
import Foreign.C.String
import Foreign.C.Types

type OffT    = {#type git_off_t#}
type TimeT   = {#type git_time_t#}

{#enum git_otype as OType {underscoreToCase}#}
{#enum git_rtype as RType {underscoreToCase}#}

