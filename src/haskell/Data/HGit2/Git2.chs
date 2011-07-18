module Data.HGit2.Git2 where

import Foreign
import Foreign.C.Types

type CPtr = Ptr ()

class CWrapper a where
  unwrap :: a -> CPtr

