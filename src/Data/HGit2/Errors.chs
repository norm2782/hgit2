{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}

#include "git2.h"

module Data.HGit2.Errors where

import Data.Bits
import Data.HGit2.Git2
import Data.Maybe
import Foreign
import Foreign.C.String
import Foreign.C.Types

lastError :: IO String
lastError = peekCString =<< {#call git_lasterror#}

clearError :: IO ()
clearError = {#call git_clearerror#}