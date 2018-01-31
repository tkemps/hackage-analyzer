{-# LANGUAGE StandaloneDeriving, DeriveAnyClass, DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-orphans -Wno-name-shadowing -Wno-unused-do-bind #-}
module Hackage.Scrape.BuildStatus where

import           Control.Applicative
import           Control.DeepSeq ()
import           Data.Aeson hiding (Result)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe
import qualified Data.Text as T
import           Protolude hiding (packageName, link, takeWhile, option)
import           Test.WebDriver (openPage, getSource, Selector(..), findElem, getText)
import           Test.WebDriver.Monad (WD)
import           Test.WebDriver.Commands.Wait (onTimeout, waitUntil)
import           Text.HTML.Scalpel

import Hackage.Scrape.PackageInfo

buildStatusFromText :: Text -> BuildStatus
buildStatusFromText b =
  if b=="stcell pass-build" then OK
  else if b=="stcell pass-no-op" then OK_boot
  else if b=="stcell pass-no-ip" then OK_no_ip
  else if b=="stcell fail-bj" then FAIL_BJ
  else if b=="stcell fail-build" then FAIL_pkg
  else if b=="stcell fail-dep-build" then FAIL_deps
  else if b=="stcell fail-no-ip" then FAIL_no_ip
  else if b=="stcell fail-unknown" then FAIL_Unknown
  else Other

buildStatus :: Scraper Text (Map GhcVersion BuildStatus)
buildStatus =
  chroot
    ("div" @: ["id" @= "package"] //
     tagSelector "table" //
     tagSelector "tbody" //
     "tr" @: [hasClass "solver-row",
              hasClass "first-major",
              hasClass "first-minor"])
    (do
        ghcVersions <- (fmap GhcVersion) <$> attrs "data-ghc-version" (tagSelector "td")
        buildStatuses <- (fmap buildStatusFromText) <$> attrs "class" (tagSelector "td")
        return (Map.fromList (zip ghcVersions buildStatuses)))

buildStatusURL :: Text -> URL
buildStatusURL packageName = "https://matrix.hackage.haskell.org/package/"++(T.unpack packageName)

scrapeBuildStatus :: Text -> WD (Maybe (Map GhcVersion BuildStatus))
scrapeBuildStatus pkg = do
  openPage (buildStatusURL pkg)
  waitUntil 5 (getText <=< findElem $ ByClass "first-major") `onTimeout` return ""
  src <- getSource
  let mBS = scrapeStringLike src buildStatus
  return mBS
