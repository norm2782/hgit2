{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}

#include <git2/net.h>

module Data.HGit2.Net where

import Data.Bits
import Data.Maybe
import Foreign
import Foreign.C.String
import Foreign.C.Types

defaultPort :: String
defaultPort = "9418" -- TODO: Import from net.h?

{#enum define Direction { GIT_DIR_FETCH as Fetch
                        , GIT_DIR_PUSH as Push}#}
