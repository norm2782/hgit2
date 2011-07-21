{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}

#include <git2/tag.h>

module Data.HGit2.Tag where

import Data.HGit2.Git2
import Data.HGit2.Repository
import Data.HGit2.OID
import Data.HGit2.Types
import Foreign
import Foreign.C

{-
/**
 * Get the id of a tag.
 *
 * @param tag a previously loaded tag.
 * @return object identity for the tag.
 */
GIT_EXTERN(const git_oid *) git_tag_id(git_tag *tag);

/**
 * Get the tagged object of a tag
 *
 * This method performs a repository lookup for the
 * given object and returns it
 *
 * @param target pointer where to store the target
 * @param tag a previously loaded tag.
 * @return 0 on success; error code otherwise
 */
GIT_EXTERN(int) git_tag_target(git_object **target, git_tag *tag);

/**
 * Get the OID of the tagged object of a tag
 *
 * @param tag a previously loaded tag.
 * @return pointer to the OID
 */
GIT_EXTERN(const git_oid *) git_tag_target_oid(git_tag *tag);

/**
 * Get the type of a tag's tagged object
 *
 * @param tag a previously loaded tag.
 * @return type of the tagged object
 */
GIT_EXTERN(git_otype) git_tag_type(git_tag *tag);

/**
 * Get the name of a tag
 *
 * @param tag a previously loaded tag.
 * @return name of the tag
 */
GIT_EXTERN(const char *) git_tag_name(git_tag *tag);

/**
 * Get the tagger (author) of a tag
 *
 * @param tag a previously loaded tag.
 * @return reference to the tag's author
 */
GIT_EXTERN(const git_signature *) git_tag_tagger(git_tag *tag);

/**
 * Get the message of a tag
 *
 * @param tag a previously loaded tag.
 * @return message of the tag
 */
GIT_EXTERN(const char *) git_tag_message(git_tag *tag);


/**
 * Create a new tag in the repository from an object
 *
 * A new reference will also be created pointing to
 * this tag object. If `force` is true and a reference
 * already exists with the given name, it'll be replaced.
 *
 * @param oid Pointer where to store the OID of the
 * newly created tag. If the tag already exists, this parameter
 * will be the oid of the existing tag, and the function will
 * return a GIT_EEXISTS error code.
 *
 * @param repo Repository where to store the tag
 *
 * @param tag_name Name for the tag; this name is validated
 * for consistency. It should also not conflict with an
 * already existing tag name
 *
 * @param target Object to which this tag points. This object
 * must belong to the given `repo`.
 *
 * @param tagger Signature of the tagger for this tag, and
 *  of the tagging time
 *
 * @param message Full message for this tag
 *
 * @param force Overwrite existing references
 *
 * @return 0 on success; error code otherwise.
 *	A tag object is written to the ODB, and a proper reference
 *	is written in the /refs/tags folder, pointing to it
 */
GIT_EXTERN(int) git_tag_create(
		git_oid *oid,
		git_repository *repo,
		const char *tag_name,
		const git_object *target,
		const git_signature *tagger,
		const char *message,
		int force);

/**
 * Create a new tag in the repository from a buffer
 *
 * @param oid Pointer where to store the OID of the newly created tag
 * @param repo Repository where to store the tag
 * @param buffer Raw tag data
 * @param force Overwrite existing tags
 * @return 0 on sucess; error code otherwise
 */
GIT_EXTERN(int) git_tag_create_frombuffer(
		git_oid *oid,
		git_repository *repo,
		const char *buffer,
		int force);

/**
 * Create a new lightweight tag pointing at a target object
 *
 * A new direct reference will be created pointing to
 * this target object. If `force` is true and a reference
 * already exists with the given name, it'll be replaced.
 *
 * @param oid Pointer where to store the OID of the provided
 * target object. If the tag already exists, this parameter
 * will be filled with the oid of the existing pointed object
 * and the function will return a GIT_EEXISTS error code.
 *
 * @param repo Repository where to store the lightweight tag
 *
 * @param tag_name Name for the tag; this name is validated
 * for consistency. It should also not conflict with an
 * already existing tag name
 *
 * @param target Object to which this tag points. This object
 * must belong to the given `repo`.
 *
 * @param force Overwrite existing references
 *
 * @return 0 on success; error code otherwise.
 *	A proper reference is written in the /refs/tags folder,
 *  pointing to the provided target object
 */
int git_tag_create_lightweight(
		git_oid *oid,
		git_repository *repo,
		const char *tag_name,
		const git_object *target,
		int force);

/**
 * Delete an existing tag reference.
 *
 * @param repo Repository where lives the tag
 *
 * @param tag_name Name of the tag to be deleted;
 * this name is validated for consistency.
 *
 * @return 0 on success; error code otherwise.
 */
GIT_EXTERN(int) git_tag_delete(
		git_repository *repo,
		const char *tag_name);

/**
 * Fill a list with all the tags in the Repository
 *
 * The string array will be filled with the names of the
 * matching tags; these values are owned by the user and
 * should be free'd manually when no longer needed, using
 * `git_strarray_free`.
 *
 * @param tag_names Pointer to a git_strarray structure where
 *		the tag names will be stored
 * @param repo Repository where to find the tags
 * @return 0 on success; error code otherwise
 */
GIT_EXTERN(int) git_tag_list(
		git_strarray *tag_names,
		git_repository *repo);

/**
 * Fill a list with all the tags in the Repository
 * which name match a defined pattern
 *
 * If an empty pattern is provided, all the tags
 * will be returned.
 *
 * The string array will be filled with the names of the
 * matching tags; these values are owned by the user and
 * should be free'd manually when no longer needed, using
 * `git_strarray_free`.
 *
 * @param tag_names Pointer to a git_strarray structure where
 *		the tag names will be stored
 * @param pattern Standard fnmatch pattern
 * @param repo Repository where to find the tags
 * @return 0 on success; error code otherwise
 */
GIT_EXTERN(int) git_tag_list_match(
		git_strarray *tag_names,
		const char *pattern,
		git_repository *repo);

-}
