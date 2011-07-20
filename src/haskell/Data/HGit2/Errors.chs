{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE StandaloneDeriving #-}

#include <git2/errors.h>

module Data.HGit2.Errors where

import Data.Maybe()
import Foreign
import Foreign.C
import Foreign.C.String

{#enum git_error as GitError {underscoreToCase}#}

deriving instance Show GitError

lastError :: IO String
lastError = peekCString =<< {#call git_lasterror#}

clearError :: IO ()
clearError = {#call git_clearerror#}
