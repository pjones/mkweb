{-# LANGUAGE OverloadedStrings #-}

{-

This file is part of the package mkweb. It is subject to the license
terms in the LICENSE file found in the top-level directory of this
distribution and at:

  git://git.devalot.com/mkweb.git

No part of this package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}

--------------------------------------------------------------------------------
module Hakyll.MkWeb.Author
       ( loadAuthors
       , authorFields
       ) where

--------------------------------------------------------------------------------
-- Library imports.
import qualified Data.Map as M
import Data.Yaml (decodeFileEither)
import Hakyll

--------------------------------------------------------------------------------
-- Project imports.
import MkWeb.Types.Author

--------------------------------------------------------------------------------
-- | Load the given authors file.
loadAuthors :: FilePath -> IO (AuthorMap)
loadAuthors file = do
  authors <- decodeFileEither file

  case authors of
    Left err -> fail ("failed to parse authors file: " ++ show err)
    Right x  -> return x

--------------------------------------------------------------------------------
-- | Locate author information for the given item.
lookupAuthor :: AuthorMap -> Item a -> Compiler Author
lookupAuthor authors item = do
    metadata <- getMetadata (itemIdentifier item)
    case lookupString "author" metadata >>= flip M.lookup authors of
      Nothing -> fail missing
      Just x  -> return x

  where
    missing :: String
    missing = (toFilePath $ itemIdentifier item) ++
              " does not have valid author information"

--------------------------------------------------------------------------------
-- | Create a context with fields for author information:
--
--   * @author@: The author's full name.
--   * @authorURL@: URL for the author's about page.
authorFields :: AuthorMap        -- ^ Loaded map of authors.
             -> Context a        -- ^ Resulting @Context@.
authorFields authors =
    mconcat [ field "author"        (fetch authorName)
            , field "authorEmail"   (fetch authorEmail)
            , field "authorURL"     (fetch authorURL)
            , field "authorTwitter" (fetch authorTwitter)
            , field "authorBio"     selectBio
            ]
  where
    fetch :: (Author -> String) -> Item a -> Compiler String
    fetch f = fmap f . lookupAuthor authors

    selectBio :: Item a -> Compiler String
    selectBio item = do
      a <- lookupAuthor authors item
      tags <- getTags (itemIdentifier item)
      bioItem <- load (fromFilePath $ bioForTag tags a)
      return (itemBody bioItem)
