{-# LANGUAGE UndecidableInstances, TemplateHaskell, Rank2Types,
             NoMonomorphismRestriction, FlexibleContexts,
             StandaloneDeriving, BangPatterns, DeriveAnyClass,
             DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-orphans -Wno-name-shadowing -Wno-unused-do-bind #-}
module Hackage.Scrape.Db where

import           Control.DeepSeq ()
import           Control.Lens
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe
import qualified Data.Set as Set
import           Data.Time.Clock
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.ToField
import           Database.PostgreSQL.Simple.ToRow
import           Database.PostgreSQL.Simple.Types
import           Protolude hiding (packageName, link, takeWhile, option)

import Hackage.Scrape.PackageInfo

instance (ToField a, ToField b, ToField c, ToField d, ToField e, ToField f,
          ToField g, ToField h, ToField i, ToField j, ToField k, ToField l, ToField m,
          ToField n, ToField o, ToField p, ToField q, ToField r, ToField s, ToField t)
    => ToRow (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t) where
    toRow (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t) =
        [toField a, toField b, toField c, toField d, toField e, toField f,
         toField g, toField h, toField i, toField j, toField k, toField l, toField m,
         toField n, toField o, toField p, toField q, toField r, toField s, toField t]

insertPackageMetaData :: Connection -> PackageInfo -> IO (Int64, Int64)
insertPackageMetaData conn pms = do
  let values = fmap (\p -> (view packageName p,
                           view version p,
                           view runDate p,
                           view dateTimeSnapshot p,
                           view totalDownloads p,
                           view thirtyDaysDownloads p,
                           (PGArray $ view tags p) :: PGArray Text,
                           view authors p,
                           view maintainers p,
                           view homepage p,
                           view bugTracker p,
                           view sourceRepository p,
                           view uploaded p,
                           view rating p,
                           view size p,
                           (PGArray (fmap show (Set.toList (view extensionsDeclared p))) :: PGArray Text),
                           (PGArray $ view versions p) :: PGArray Text,
                           (PGArray $ view categories p) :: PGArray Text,
                           view link p,
                           view description p)) [pms]
  n1 <- executeMany conn "insert into hackage.package_snapshot (\
                   \ package_name, version, run_date, \
                   \ date_time_snapshot,\
                   \ downloads_total, downloads_30days,\
                   \ tags, authors, maintainers, \
                   \ homepage, bug_tracker, source_repository, \
                   \ uploaded, rating, size, extensions_declared, versions, \
                   \ categories, link, description)\
                   \ values (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)" values
  let depValues = fmap (\d -> (view packageName pms,
                               view version pms,
                               view runDate pms,
                               view _1 d,
                               fromMaybe "" (view _2 d))) (view dependencies pms)
  n2 <- executeMany conn "insert into hackage.dependency (\
                   \ package_name, version, run_date,\
                   \ package_name_dependency, version_dependency) \
                   \ values (?,?,?,?,?)" depValues
  return (n1,n2)


insertBuildStatus :: Connection -> PackageInfo -> Map GhcVersion BuildStatus -> IO Int64
insertBuildStatus conn p bs = do
  let bsl = Map.toList bs
  let values = fmap (\(v,s) -> (view packageName p,
                                view version p,
                                view runDate p,
                                ghcVersionToText v,
                                show s :: Text)) bsl
  n <- executeMany conn "insert into hackage.build_status (\
                   \ package_name, version, run_date, \
                   \ ghc_version, build_status)\
                   \ values (?,?,?,?,?)" values
  return n

deleteRunDateFromDb :: Connection -> UTCTime -> IO Int64
deleteRunDateFromDb conn runDate = do
  execute conn "delete from hackage.dependency where run_date=?" [runDate]
  execute conn "delete from hackage.build_status where run_date=?" [runDate]
  execute conn "delete from hackage.package_snapshot where run_date=?" [runDate]
