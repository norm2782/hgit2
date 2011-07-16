{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}

#include <git2/odb.h>

module Data.HGit2.ODB where

import Data.HGit2.Git2

newtype ODB = ODB CPtr

{#enum git_odb_streammode as ODBStreamMode {underscoreToCase}#}

