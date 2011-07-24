{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}

#include <git2/config.h>

module Data.HGit2.Config where

import Data.HGit2.Git2
import Data.HGit2.Errors
import Foreign
import Foreign.C

newtype Config = Config CPtr
newtype ConfigFile = ConfigFile CPtr


instance CWrapper Config where
  unwrap (Config c) = c

instance CWrapper ConfigFile where
  unwrap (ConfigFile cf) = cf

-- | Locate the path to the global configuration file
--
-- The user or global configuration file is usually located in
-- `$HOME/.gitconfig`.
--
-- This method will try to guess the full path to that file, if the file
-- exists. The returned path may be used on any `git_config` call to load the
-- global configuration file.
findGlobalConfig :: IO (Maybe String)
findGlobalConfig = alloca $ \pth -> do
  res <- {#call git_config_find_global#} pth
  if res == 0
    then fmap Just $ peekCString pth
    else return Nothing

-- | Open the global configuration file
openGlobalConfig :: IOEitherErr Config
openGlobalConfig = callPeek Config {#call git_config_open_global#}

-- | Create a configuration file backend for ondisk files
--
-- These are the normal `.gitconfig` files that Core Git processes. Note that
-- you first have to add this file to a configuration object before you can
-- query it for configuration variables.
createOnDisk :: String -> IOEitherErr ConfigFile
createOnDisk str = withCString str $ \str' ->
  callPeek ConfigFile (\out -> {#call git_config_file__ondisk#} out str')

-- | Allocate a new configuration object
--
-- This object is empty, so you have to add a file to it before you can do
-- anything with it.
newConfig :: IOEitherErr Config
newConfig = callPeek Config {#call git_config_new#}

-- | Add a generic config file instance to an existing config
--
-- Note that the configuration object will free the file automatically.
--
-- Further queries on this config object will access each of the config file
-- instances in order. Instances with a higher priority will be accessed first.
addFile :: Config -> ConfigFile -> Int -> IO GitError
addFile (Config c) (ConfigFile f) pr =
  retEnum $ {#call git_config_add_file#} c f (fromIntegral pr)


-- | Add an on-disk config file instance to an existing config
--
-- The on-disk file pointed at by `path` will be opened and parsed; it's
-- expected to be a native Git config file following the default Git config
-- syntax (see man git-config).
--
-- Note that the configuration object will free the file automatically.
--
-- Further queries on this config object will access each of the config file
-- instances in order. Instances with a higher priority will be accessed first.
addOnDisk :: Config -> String -> Int -> IO GitError
addOnDisk (Config c) pth pr = withCString pth $ \pth' ->
  retEnum $ {#call git_config_add_file_ondisk#} c pth' (fromIntegral pr)

-- | Create a new config instance containing a single on-disk file
--
-- This method is a simple utility wrapper for the following sequence of calls:
-- - newConfig
-- - addOnDisk
openOnDisk :: String -> IOEitherErr Config
openOnDisk str = withCString str $ \str' ->
  callPeek Config (\out -> {#call git_config_open_ondisk#} out str')

-- | Get the value of an integer config variable.
configInt :: Config -> String -> IOEitherErr Int
configInt (Config c) str = withCString str $ \str' ->
  callPeek fromIntegral ({#call git_config_get_int#} c str')

-- | Get the value of an integer config variable.
configInteger :: Config -> String -> IOEitherErr Integer
configInteger (Config c) str = withCString str $ \str' ->
  callPeek fromIntegral ({#call git_config_get_long#} c str')

-- | Get the value of a boolean config variable.
configBool :: Config -> String -> IO (Either GitError Bool)
configBool (Config c) str = withCString str $ \str' -> alloca $ \out -> do
  res  <- {#call git_config_get_bool#} c str' out
  retEither res $ fmap (Right . toBool) $ peek out

-- | Get the value of a string config variable.
--
-- The string is owned by the variable and should not be freed by the user.
configString :: Config -> String -> IO (Either GitError String)
configString (Config c) vn = withCString vn $ \vn' -> alloca $ \out -> do
  res <- {#call git_config_get_string#} c vn' out
  retEither res $ fmap Right $ peekCString =<< peek out

-- | Set the value of an integer config variable.
setConfigInt :: Config -> String -> Int -> IOCanFail
setConfigInt conf vn val =
  mConfig {#call git_config_set_int#} conf vn (fromIntegral val)

-- | Set the value of a long integer config variable.
setConfigInteger :: Config -> String -> Integer -> IOCanFail
setConfigInteger conf vn val =
  mConfig {#call git_config_set_long#} conf vn (fromIntegral val)

-- | Set the value of a boolean config variable.
setConfigBool :: Config -> String -> Bool -> IOCanFail
setConfigBool conf vn val =
  mConfig {#call git_config_set_bool#} conf vn (fromBool val)

-- | Set the value of a string config variable.
setConfigString :: Config -> String -> String -> IOCanFail
setConfigString conf vn val = withCString val $ \val' ->
  mConfig {#call git_config_set_string#} conf vn val'

-- | Delete a config variable
delConfig :: Config -> String -> IOCanFail
delConfig (Config c) vn = withCString vn $ \str ->
  retMaybe =<< {#call git_config_delete#} c str

mConfig :: (CPtr -> CString -> t -> IO CInt) -> Config -> String -> t
        -> IO (Maybe GitError)
mConfig call (Config c) vn val = withCString vn $ \str ->
  retMaybe =<< call c str val

-- TODO: foreachConfig :: Config ->
{-
/**
 * Perform an operation on each config variable.
 *
 * The callback receives the normalized name and value of each variable
 * in the config backend, and the data pointer passed to this function.
 * As soon as one of the callback functions returns something other than 0,
 * this function returns that value.
 *
 * @param cfg where to get the variables from
 * @param callback the function to call on each variable
 * @param payload the data to pass to the callback
 * @return GIT_SUCCESS or the return value of the callback which didn't return 0
 */
GIT_EXTERN(int) git_config_foreach(
	git_config *cfg,
	int (*callback)(const char *var_name, const char *value, void *payload),
	void *payload);
-}

