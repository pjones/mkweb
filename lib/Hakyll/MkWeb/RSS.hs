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
module Hakyll.MkWeb.RSS
       ( feedSettings
       , createRSS
       )where

--------------------------------------------------------------------------------
import Control.Applicative (empty)
import Data.Char (isSpace)
import Hakyll
import System.FilePath (dropExtension)

--------------------------------------------------------------------------------
import Hakyll.MkWeb.Base

--------------------------------------------------------------------------------
feedSettings :: IO FeedConfiguration
feedSettings = do
  tagline <- readFile "templates/tagline.txt"

  return $ FeedConfiguration
    { feedTitle       = "Devalot"
    , feedDescription = reverse $ dropWhile isSpace $ reverse tagline
    , feedAuthorName  = "Peter J. Jones"
    , feedAuthorEmail = "pjones@devalot.com"
    , feedRoot        = "http://www.devalot.com"
    }

--------------------------------------------------------------------------------
renderDevalotFeed :: FeedConfiguration       -- ^ Feed configuration
                  -> BaseContext             -- ^ Base Context.
                  -> Tags                    -- ^ Tag mapping.
                  -> [Item String]           -- ^ Input items
                  -> Compiler (Item String)  -- ^ Resulting item
renderDevalotFeed config base tags items =
    makeItem "" >>= loadAndApplyTemplate "templates/rss.xml" feedContext

  where
    itemContext = mconcat [ constField "root" (feedRoot config)
                          , dateField "published" "%a, %d %b %Y 08:00:00 MST"
                          , bodyField "description"
                          , fixedTags (tagsField "topics" tags)
                          , guidField
                          , articleCtx base
                          ]

    feedContext = mconcat [ bodyField  "body"
                          , constField "title"       (feedTitle config)
                          , constField "description" (feedDescription config)
                          , constField "authorName"  (feedAuthorName config)
                          , constField "authorEmail" (feedAuthorEmail config)
                          , constField "root"        (feedRoot config)
                          , urlField   "url"
                          , listField "items" itemContext (return $ fixedItems items)
                          , updatedField
                          , missingField
                          ]

    -- Replace all relative links with absolute links.
    fixedItems :: [Item String] -> [Item String]
    fixedItems = map (fmap (relativizeUrlsWith $ feedRoot config))

    -- Same as fixedItems but for the tags.
    fixedTags :: Context a -> Context a
    fixedTags = mapContext (relativizeUrlsWith $ feedRoot config)

    -- Updated time for the entire feed from the most recent item.
    updatedField = field "updated" $ \_ -> case items of
      []    -> return "Unknown"
      (x:_) -> unwrapCtx itemContext "published" x

    -- Mimic the older Devalot.com item GUIDs.
    guidField = field "guid" $ \i -> do
      url <- fmap (maybe empty toUrl) . getRoute . itemIdentifier $ i
      return $ feedRoot config ++ dropExtension url

    -- Unwrap a context.  Necessary because the new ContextField
    -- type in Hakyll.
    unwrapCtx :: Context a -> String -> Item a -> Compiler String
    unwrapCtx ctx name item = do
      ctxfield <- unContext ctx name [] item

      case ctxfield of
        StringField x        -> return x
        ListField _ []       -> return ""
        ListField ctx' (x:_) -> unwrapCtx ctx' name x

--------------------------------------------------------------------------------
createRSS :: FeedConfiguration -> BaseContext -> Tags -> Rules ()
createRSS cfg ctx tags =
  create ["feeds/all.rss"] $ do
    route idRoute
    compile $ do
      ls <- take 10 . reverse <$> (snapshots >>= chronItems)
      renderDevalotFeed cfg ctx tags ls
  where pat       = "content/articles/**"
        snapshots = loadAllSnapshots pat "content"
