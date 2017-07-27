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
module Hakyll.MkWeb.Archive (createArchive) where

--------------------------------------------------------------------------------
import Hakyll

--------------------------------------------------------------------------------
import Hakyll.MkWeb.Base

--------------------------------------------------------------------------------
-- | Context for a single archive (year) page.
archiveCtx :: Context String
archiveCtx =
  mconcat [ field "url" url
          , field "articles" count
          , bodyField "year"
          , defaultContext
          ]
  where
    -- Correct URL to the page.
    url :: Item String -> Compiler String
    url i = return $ "archive/" ++ itemBody i ++ ".html"

    -- The number of articles.
    count :: Item String -> Compiler String
    count = fmap (show . length) . list

    -- A way to count the articles.
    list :: Item String -> Compiler [Item String]
    list i = loadAll . fromGlob $ "content/articles/" ++ itemBody i ++ "/**"

--------------------------------------------------------------------------------
-- | Create an article archive index and one archive page per year.
createArchive :: BaseContext -> [String] -> Rules ()
createArchive base years = do
  create ["articles/archive.html"] $ do
    route idRoute
    compile $ makeItem ""
      >>= loadAndApplyTemplate "templates/archives.html" ctx
      >>= relativizeUrls

  -- Create one archive file per year.
  mapM_ (createArchiveYear base) years

  where
    -- Context for the archive index.
    ctx :: Context String
    ctx = mconcat [ constField "title" "Article Archive"
                  , listField "years" archiveCtx (mapM makeItem years)
                  , defaultContext
                  ]

--------------------------------------------------------------------------------
-- | Create an article archive page for the given year.
createArchiveYear :: BaseContext -> String -> Rules ()
createArchiveYear base year = create [fromFilePath path] $ do
  route (setExtension ".html")
  compile $ makeItem ""
    >>= loadAndApplyTemplate "templates/archive.html" ctx
    >>= relativizeUrls
  where
    path :: String
    path = "articles/archive/" ++ year

    glob :: Pattern
    glob = fromGlob ("content/articles/" ++ year ++ "/**")

    title :: String
    title = year ++ " Article Archive"

    articles :: Compiler [Item String]
    articles = loadAll glob >>= chronItems

    ctx :: Context String
    ctx = mconcat [ constField "title" title
                  , listField "articles" (articleCtx base) articles
                  , articleCtx base
                  ]
