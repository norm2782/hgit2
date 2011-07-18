{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}

#include <git2/config.h>

module Data.HGit2.Config where

import Data.HGit2.Git2
import Data.HGit2.Errors
import Foreign
import Foreign.C

newtype Config = Config CPtr

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
openGlobalConfig :: IO (Either GitError Config)
openGlobalConfig = alloca $ \conf -> do
  res <- {#call git_config_open_global#} conf
  retEither res $ fmap (Right . Config) $ peek conf

{-
/**
 * Create a configuration file backend for ondisk files
 *
 * These are the normal `.gitconfig` files that Core Git
 * processes. Note that you first have to add this file to a
 * configuration object before you can query it for configuration
 * variables.
 *
 * @param out the new backend
 * @param path where the config file is located
 */
GIT_EXTERN(int) git_config_file__ondisk(struct git_config_file **out, const char *path);

/**
 * Allocate a new configuration object
 *
 * This object is empty, so you have to add a file to it before you
 * can do anything with it.
 *
 * @param out pointer to the new configuration
 * @return GIT_SUCCESS on success; error code otherwise
 */
GIT_EXTERN(int) git_config_new(git_config **out);

/**
 * Add a generic config file instance to an existing config
 *
 * Note that the configuration object will free the file
 * automatically.
 *
 * Further queries on this config object will access each
 * of the config file instances in order (instances with
 * a higher priority will be accessed first).
 *
 * @param cfg the configuration to add the file to
 * @param file the configuration file (backend) to add
 * @param priority the priority the backend should have
 * @return GIT_SUCCESS on success; error code otherwise
 */
GIT_EXTERN(int) git_config_add_file(git_config *cfg, git_config_file *file, int priority);

/**
 * Add an on-disk config file instance to an existing config
 *
 * The on-disk file pointed at by `path` will be opened and
 * parsed; it's expected to be a native Git config file following
 * the default Git config syntax (see man git-config).
 *
 * Note that the configuration object will free the file
 * automatically.
 *
 * Further queries on this config object will access each
 * of the config file instances in order (instances with
 * a higher priority will be accessed first).
 *
 * @param cfg the configuration to add the file to
 * @param path path to the configuration file (backend) to add
 * @param priority the priority the backend should have
 * @return GIT_SUCCESS on success; error code otherwise
 */
GIT_EXTERN(int) git_config_add_file_ondisk(git_config *cfg, const char *path, int priority);


/**
 * Create a new config instance containing a single on-disk file
 *
 * This method is a simple utility wrapper for the following sequence
 * of calls:
 *	- git_config_new
 *	- git_config_add_file_ondisk
 *
 * @param cfg The configuration instance to create
 * @param path Path to the on-disk file to open
 * @return GIT_SUCCESS on success; error code otherwise
 */
GIT_EXTERN(int) git_config_open_ondisk(git_config **cfg, const char *path);

/**
 * Free the configuration and its associated memory and files
 *
 * @param cfg the configuration to free
 */
GIT_EXTERN(void) git_config_free(git_config *cfg);

/**
 * Get the value of an integer config variable.
 *
 * @param cfg where to look for the variable
 * @param name the variable's name
 * @param out pointer to the variable where the value should be stored
 * @return GIT_SUCCESS on success; error code otherwise
 */
GIT_EXTERN(int) git_config_get_int(git_config *cfg, const char *name, int *out);

/**
 * Get the value of a long integer config variable.
 *
 * @param cfg where to look for the variable
 * @param name the variable's name
 * @param out pointer to the variable where the value should be stored
 * @return GIT_SUCCESS on success; error code otherwise
 */
GIT_EXTERN(int) git_config_get_long(git_config *cfg, const char *name, long int *out);

/**
 * Get the value of a boolean config variable.
 *
 * This function uses the usual C convention of 0 being false and
 * anything else true.
 *
 * @param cfg where to look for the variable
 * @param name the variable's name
 * @param out pointer to the variable where the value should be stored
 * @return GIT_SUCCESS on success; error code otherwise
 */
GIT_EXTERN(int) git_config_get_bool(git_config *cfg, const char *name, int *out);
-}

-- | Get the value of a string config variable.
-- The string is owned by the variable and should not be freed by the user.
configString :: Config -> String -> IO (Either GitError String)
configString (Config c) vn = alloca $ \out -> do
  vn' <- newCString vn
  res <- {#call git_config_get_string#} c vn' out
  retEither res $ fmap Right $ peekCString =<< peek out

-- | Set the value of an integer config variable.
setConfigInt :: Config -> String -> Int -> IO (Maybe GitError)
setConfigInt (Config c) vn val = do
  str <- newCString vn
  retMaybe =<< {#call git_config_set_int#} c str (fromIntegral val)

-- | Set the value of a long integer config variable.
setConfigInteger :: Config -> String -> Integer -> IO (Maybe GitError)
setConfigInteger (Config c) vn val = do
  str <- newCString vn
  retMaybe =<< {#call git_config_set_long#} c str (fromIntegral val)

-- | Set the value of a boolean config variable.
setConfigBool :: Config -> String -> Bool -> IO (Maybe GitError)
setConfigBool (Config c) vn val = do
  str <- newCString vn
  retMaybe =<< {#call git_config_set_bool#} c str (fromBool val)

-- | Set the value of a string config variable.
setConfigString :: Config -> String -> String -> IO (Maybe GitError)
setConfigString (Config c) vn val = do
  str <- newCString vn
  vl' <- newCString val
  retMaybe =<< {#call git_config_set_string#} c str vl'

-- | Delete a config variable
delConfig :: Config -> String -> IO (Maybe GitError)
delConfig (Config c) vn = do
  str <- newCString vn
  retMaybe =<< {#call git_config_delete#} c str

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

