{-# LANGUAGE TemplateHaskell, StandaloneDeriving, DeriveAnyClass, DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Hackage.Scrape.PackageInfo where

import           Control.Lens
import           Data.Aeson
import           Data.Map (Map)
import           Data.Maybe
import           Data.Set (Set)
import           Data.Time.Clock
import           Language.Haskell.Exts
import           Protolude
import           Text.HTML.Scalpel

data BuildStatus = OK | OK_boot | OK_no_ip | FAIL_BJ | FAIL_pkg |
                   FAIL_deps | FAIL_no_ip | FAIL_Unknown | Other
  deriving Show

data GhcVersion = GhcVersion {ghcVersionToText :: Text} deriving (Eq, Ord, Show)

deriving instance Generic GhcVersion
deriving instance Generic BuildStatus

instance ToJSON GhcVersion where
instance ToJSONKey GhcVersion
instance FromJSONKey GhcVersion
instance FromJSON GhcVersion

instance ToJSON BuildStatus where
instance FromJSON BuildStatus

data PackageInfo = PackageInfo {
  _packageName :: Text,
  _version :: Text,
  _runDate :: UTCTime,
  _dateTimeSnapshot :: UTCTime,
  _dependencies :: [(Text, Maybe Text)],
  _tags :: [Text],
  _description :: Text,
  _link :: URL,
  _versions :: [Text],
  _categories :: [Text],
  _extensionsDeclared :: Set Extension,
  _totalDownloads :: Int,
  _thirtyDaysDownloads :: Int,
  _authors :: Text,
  _maintainers :: Text,
  _homepage :: Text,
  _bugTracker :: Text,
  _sourceRepository :: Text,
  _uploaded :: UTCTime,
  _rating :: Maybe Double,
  _size :: Int64,
  _packageBuildStatus :: Maybe (Map GhcVersion BuildStatus)}
  deriving (Show,Generic)

makeLenses ''PackageInfo

deriving instance Generic KnownExtension
deriving instance Generic Extension
instance ToJSON KnownExtension
instance FromJSON KnownExtension
instance ToJSON Extension
instance FromJSON Extension
instance ToJSON PackageInfo where
instance FromJSON PackageInfo

