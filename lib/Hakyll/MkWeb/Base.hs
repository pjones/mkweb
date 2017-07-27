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
module Hakyll.MkWeb.Base
       ( BaseContext
       , loadBaseContext
       , baseContext
       , articleCtx
       , chronItems
       , articleYears
       , stripContentDir
       , sass
       , createJS
       , edify
       ) where

--------------------------------------------------------------------------------
-- Library imports.
import Control.Monad (forM)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.List (sort, sortBy)
import Data.Monoid ((<>))
import Data.Ord (comparing)
import Data.Text (unpack)
import Data.Text.Encoding (decodeUtf8)
import Hakyll
import System.Directory (getDirectoryContents)
import Data.Time.Format (defaultTimeLocale)
import Text.Jasmine
import qualified Text.Edify.Filter as Edify

--------------------------------------------------------------------------------
-- Project imports.
import Hakyll.MkWeb.Author (loadAuthors, authorFields)

--------------------------------------------------------------------------------
-- | Type to keep the base context from leaking out.
newtype BaseContext = BC (Context String)

--------------------------------------------------------------------------------
loadBaseContext :: IO BaseContext
loadBaseContext = do
  authors <- loadAuthors "etc/authors.yml"
  return $ BC (authorFields authors <> defaultContext)

--------------------------------------------------------------------------------
baseContext :: BaseContext -> Context String
baseContext (BC ctx) = ctx

--------------------------------------------------------------------------------
articleCtx :: BaseContext -> Context String
articleCtx bc = dateField "date" "%B %e, %Y" <>
                dateField "year" "%Y"        <>
                baseContext bc

--------------------------------------------------------------------------------
chronItems :: [Item a] -> Compiler [Item a]
chronItems items = do
  withTime <- forM items $ \item -> do
    utc <- getItemUTC defaultTimeLocale $ itemIdentifier item
    return (utc, item)
  return $ map snd $ sortBy (comparing fst) withTime

--------------------------------------------------------------------------------
articleYears :: IO [String]
articleYears = do
  ls <- getDirectoryContents "content/articles"
  return . sort $ filter (\x -> x /= "." && x /= "..") ls

--------------------------------------------------------------------------------
stripContentDir :: Routes
stripContentDir = setExtension "html" `composeRoutes`
                  gsubRoute "content/" (const "")

--------------------------------------------------------------------------------
sass :: Item String -> Compiler (Item String)
sass item = withItemBody (unixFilter "script/sass.sh" ops) item
            >>= return . fmap compressCss
    where ops = [css]
          css = "assets/stylesheets/devalot.scss"

--------------------------------------------------------------------------------
createJS :: Rules ()
createJS = do
  match filepattern (compile copyFileCompiler)
  create ["assets/javascript/devalot.js"] $ do
    route idRoute
    compile $ jsmin filepattern
  where filepattern = "assets/javascript/*.js"

--------------------------------------------------------------------------------
jsmin :: Pattern -> Compiler (Item String)
jsmin filepattern = do
  destPattern <- (fromGlob . toFilePath) <$> getUnderlying
  files <- getMatches (filepattern .&&. complement destPattern)
  jsData <- unsafeCompiler $ mapM (minifyFile . toFilePath) files
  makeItem . unpack $ decodeUtf8 (B.concat . L.toChunks $ L.intercalate ";" jsData)

--------------------------------------------------------------------------------
edify :: Compiler (Item String)
edify = cached cacheName $ do
  pandoc  <- getResourceBody >>= readPandocWith ropt
  pandoc' <- unsafeCompiler $ Edify.filterPandoc (itemBody pandoc)
  writePandocWith wopt <$> makeItem pandoc'
  where cacheName = "Hakyll.Web.Page.Edify"
        ropt      = defaultHakyllReaderOptions
        wopt      = defaultHakyllWriterOptions
