{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans -Wno-name-shadowing -Wno-unused-do-bind #-}
module Hackage.Scrape.CompleteList where

import           Control.DeepSeq ()
import           Control.Lens
import           Data.Char (toLower, )
import           Data.Maybe
import qualified Data.String as Str
import qualified Data.Text as T
import           Language.Haskell.TH.Syntax (mkName, nameBase)
import           Protolude hiding (packageName, link, takeWhile, option)
import           Text.HTML.Scalpel

underscorePrefixNamer :: Str.String -> FieldNamer
underscorePrefixNamer prefix _ _ n =
  case nameBase n of
    '_':x:xs -> [TopName (mkName (prefix ++ toLower x:xs))]
    _        -> []

data PackageShortInfo = PackageShortInfo {
  _sPackageName :: Text,
  _sTags :: [Text],
  _sDescription :: Text,
  _sLink :: URL
  }
  deriving (Show)

makeLenses ''PackageShortInfo

packageListItems :: Scraper Text [PackageShortInfo]
packageListItems = chroot ("ul" @: [hasClass "packages"]) packageItem

packageItem :: Scraper Text [PackageShortInfo]
packageItem = chroots (tagSelector "li") $ do
  pName <- text (tagSelector "a")
  pRelLink <- fmap T.unpack $ attr "href" (tagSelector "a")
  pDescr <- fmap (T.strip . (T.drop 1) . (T.takeWhile (/='(')) . (T.dropWhile (/=':')) . (headDef "")) $ texts anySelector
  pTags <- fmap (tailDef []) $ texts (tagSelector "a")
  return $ PackageShortInfo pName pTags pDescr ("https://hackage.haskell.org"++pRelLink)

hackageCompleteList :: URL
hackageCompleteList = T.unpack "https://hackage.haskell.org/packages/names"

scrapeHackageCompleteList :: Scraper Text a -> IO (Maybe a)
scrapeHackageCompleteList scraper = scrapeURL hackageCompleteList scraper
