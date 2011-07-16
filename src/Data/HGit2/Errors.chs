{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE StandaloneDeriving #-}

#include <git2/errors.h>

module Data.HGit2.Errors where

import Data.Maybe()
import Foreign
import Foreign.C.String
import Foreign.C.Types

{#enum git_error as GitError {underscoreToCase}#}

deriving instance Show GitError

retEither :: CInt -> IO (Either GitError a) -> IO (Either GitError a)
retEither res f | res == 0  = f
                | otherwise = return . Left . toEnum . fromIntegral $ res


retMaybeRes :: CInt -> IO (Maybe GitError)
retMaybeRes res | res == 0  = return Nothing
                | otherwise = return $ Just . toEnum . fromIntegral $ res

lastError :: IO String
lastError = peekCString =<< {#call git_lasterror#}

clearError :: IO ()
clearError = {#call git_clearerror#}
