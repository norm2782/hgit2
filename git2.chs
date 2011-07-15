{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE CPP #-}

#include "git2.h"

module Git2 where

import Foreign
import Foreign.C.Types

type OffT  = {#type git_off_t#}
type TimeT = {#type git_time_t#}
type Addr  = Ptr ()

newtype ODB = ODB { unODB :: Addr }

{#enum git_otype as OType {underscoreToCase}#}
{#enum git_rtype as RType {underscoreToCase}#}
{#enum git_repository_pathid as RepositoryPathID {underscoreToCase}#}
{#enum git_error as GitError {underscoreToCase}#}
{#enum git_odb_streammode as ODBStreamMode {underscoreToCase}#}
