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
module Main where

--------------------------------------------------------------------------------
import Data.Monoid ((<>))
import Hakyll.MkWeb.Archive (createArchive)
import Hakyll.MkWeb.Base
import Hakyll.MkWeb.RSS (feedSettings, createRSS)
import Hakyll.MkWeb.Tags (loadTags, createTags)
import Hakyll

--------------------------------------------------------------------------------
cfg :: Configuration
cfg = defaultConfiguration
  { destinationDirectory = "www"
  , storeDirectory       = ".cache"
  , tmpDirectory         = ".cache/tmp"
  }

--------------------------------------------------------------------------------
main :: IO ()
main = do
  years   <- articleYears
  feedCfg <- feedSettings
  base    <- loadBaseContext

  hakyllWith cfg $ do
    -- Compile all the templates.
    match "templates/*" $ compile templateCompiler

    -- Author blurbs.  Never actually exported to the site, just
    -- cached so they can be added to places like the end of articles.
    match "authors/**" (compile $ edify >>= saveSnapshot "content")
    -- match "etc/authors.yml" (compile $ getResourceBody >>= saveSnapshot "content")

    -- Assets (images, code, etc.)
    let assets = "assets/images/**"    .||.
                 "assets/articles/**"  .||.
                 "assets/workshops/**"

    -- Files to copy into the www directory.
    match assets $ do
      route idRoute
      compile copyFileCompiler

    -- Special code for compiling scss files.
    let scssPat = "assets/stylesheets/*.scss"
    scssDependencies <- makePatternDependency scssPat
    rulesExtraDependencies [scssDependencies] $
      match "assets/stylesheets/devalot.scss" $ do
        route (setExtension ".css")
        compile (getResourceString >>= sass)

    -- Compress all JavaScript files into a single file.
    createJS

    -- Main entry point (index.html)
    match "content/index.html" $ do
      route stripContentDir
      compile $ getResourceBody
        >>= loadAndApplyTemplate "templates/frontpage.html" (baseContext base)
        >>= relativizeUrls

    -- Contact Us.
    match "content/contact.html" $ do
      route stripContentDir
      compile $ getResourceBody
        >>= loadAndApplyTemplate "templates/default.html" (baseContext base)
        >>= relativizeUrls

    -- Guides, workshops, and other pages.
    let generic = "content/guides/**"    .||.
                  "content/workshops/**" .||.
                  "content/about/**"

    match generic $ do
      route stripContentDir
      compile $ edify
        >>= saveSnapshot "content"
        >>= loadAndApplyTemplate "templates/default.html" (baseContext base)
        >>= relativizeUrls

    -- Create a topic index and one file per topic (tag).
    tags <- loadTags
    createTags base tags

    match "content/articles/**" $ do
      let ctx = tagsField "tagLinks" tags <> articleCtx base
      route stripContentDir
      compile $ edify
        >>= saveSnapshot "content"
        >>= loadAndApplyTemplate "templates/article.html" ctx
        >>= relativizeUrls

    -- Recent article listing.
    create ["articles/index.html"] $ do
      route idRoute
      compile $ do
        let articles = (take 10 . reverse) <$>
                       (chronItems =<< loadAll "content/articles/**")

            ctx = mconcat [ constField "title" "Recent Articles"
                          , listField "articles" (articleCtx base) articles
                          , baseContext base
                          ]

        makeItem ""
          >>= loadAndApplyTemplate "templates/articles.html" ctx
          >>= relativizeUrls

    -- Create an archive index, and one archive per year.
    createArchive base years

    -- Create an RSS feed for the site.
    createRSS feedCfg base tags
