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
module Hakyll.MkWeb.Tags where

--------------------------------------------------------------------------------
import Data.Char
import Data.Monoid ((<>))
import Hakyll

--------------------------------------------------------------------------------
import Hakyll.MkWeb.Base

--------------------------------------------------------------------------------
-- Convert a tag name to a file path (Hakyll Identifier).
tagToFile :: String -> Identifier
tagToFile = fromFilePath . ("topics/" ++) . map fix
    where fix c = if isSpace c then '-' else toLower c

--------------------------------------------------------------------------------
-- Like @tagToFile@ but append the given file extension.
tagWithExt :: String -> String -> Identifier
tagWithExt ext = fromFilePath . (++ ext) . toFilePath . tagToFile

--------------------------------------------------------------------------------
-- Returns the file name for a tag's markdown file as a file pattern.
tagMarkdownFile :: String -> Pattern
tagMarkdownFile = fromGlob . ("content/" ++) . toFilePath . tagWithExt ".md"

--------------------------------------------------------------------------------
-- Load all tags from all articles.
loadTags :: MonadMetadata m => m Tags
loadTags = buildTags "content/articles/**" $ tagWithExt ".html"

--------------------------------------------------------------------------------
createSingleTag :: BaseContext -> String -> Pattern -> Rules ()
createSingleTag base tag pattern = do
  route idRoute
  compile $ do
    markdown <- loadAll $ tagMarkdownFile tag
    makeItem ""
      >>= loadAndApplyTemplate "templates/topic.html" (tagContext markdown)
      >>= relativizeUrls
  where
    tagContext :: [Item String] -> Context String
    tagContext [] = baseTagContext
    tagContext xs = constField "intro" (concatMap itemBody xs) <>
                    constField "related" "Related Articles"    <>
                    baseTagContext

    baseTagContext :: Context String
    baseTagContext =
      mconcat [ constField "title" tag
              , listField "articles" (articleCtx base)
                                     (loadAll pattern >>= chronItems)
              , defaultContext
              ]

--------------------------------------------------------------------------------
createTags :: BaseContext -> Tags -> Rules ()
createTags base tags = do
  -- Prepare the markdown for tags.
  match "content/topics/*" (compile edify)

  -- One file per tag.
  tagsRules tags (createSingleTag base)

  -- The tag cloud.
  create ["topics/index.html"] $ do
    let ctx = constField "title" "Devalot Topics" <> defaultContext
    route idRoute
    compile $ renderTagCloud 100.0 200.0 tags
      >>= makeItem
      >>= loadAndApplyTemplate "templates/topics.html" ctx
      >>= relativizeUrls
