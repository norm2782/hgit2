{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}

#include <git2.h>

module Data.HGit2.Types where

import Data.HGit2.Git2
import Foreign.C.Types

type OffT   = {#type git_off_t#}
type TimeT  = {#type git_time_t#}

newtype RefSpec = RefSpec CPtr

instance CWrapper RefSpec where
  unwrap (RefSpec r) = r

{#enum git_otype as OType {underscoreToCase}#}
{#enum git_rtype as RType {underscoreToCase}#}

