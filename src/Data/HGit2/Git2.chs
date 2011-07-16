module Data.HGit2.Git2 where

import Foreign
import Foreign.C.Types

type CPtr    = Ptr ()
type Ptr2Int = CPtr -> IO CInt
