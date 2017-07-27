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
module MkWeb.Types.Author
       ( Author (..)
       , AuthorMap
       , AuthorTag (..)
       , bioForTag
       ) where

--------------------------------------------------------------------------------
import Data.Aeson
import Data.Map (Map)
import Data.Maybe

--------------------------------------------------------------------------------
-- | Information about a Devalot author.
data Author = Author
  { authorName :: String
    -- ^ Author full name.

  , authorEmail :: String
    -- ^ Author email address.  Used in RSS feeds.

  , authorURL :: String
    -- ^ URL pointing to an "about this author" page.

  , authorBio :: String
    -- ^ Path to a local markdown file containing a short blurb on the
    -- author.

  , authorTwitter :: String
    -- ^ Full author twitter user name, including the at sign.

  , authorTags :: [AuthorTag]
    -- ^ List of author tags that can override items from above.
  }

--------------------------------------------------------------------------------
-- | A mapping from author ID to Author.
type AuthorMap = Map String Author

--------------------------------------------------------------------------------
data AuthorTag = AuthorTag
  { authorTagName :: String -- ^ Tag name, should match website tag.
  , authorTagBio  :: Maybe String -- ^ Path to bio markdown page.
  }

--------------------------------------------------------------------------------
matchingTags :: [String] -> Author -> [AuthorTag]
matchingTags tags = filter match . authorTags
  where
    match :: AuthorTag -> Bool
    match a = authorTagName a `elem` tags

--------------------------------------------------------------------------------
-- | Returns the path to the author's bio markdown page based on the
-- list of given tags.  The first matching tag is used.  If no tags
-- match then the default bio is returned instead.
bioForTag :: [String] -> Author -> String
bioForTag tags author = maybe (authorBio author) id $ do
  matches <- sequence (map authorTagBio $ matchingTags tags author)
  listToMaybe matches

--------------------------------------------------------------------------------
-- Instances.
instance FromJSON Author where
  parseJSON (Object v) = Author <$> v .: "name"
                                <*> v .: "email"
                                <*> v .: "url"
                                <*> v .: "bio"
                                <*> v .: "twitter"
                                <*> v .: "tags"

  parseJSON _ = fail "invalid author record shape"

--------------------------------------------------------------------------------
instance FromJSON AuthorTag where
  parseJSON (Object v) = AuthorTag <$> v .: "name"
                                   <*> v .: "bio"

  parseJSON _ = fail "invalid author tag record shape"
