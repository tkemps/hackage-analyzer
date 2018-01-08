{-# LANGUAGE UndecidableInstances, TemplateHaskell, Rank2Types, NoMonomorphismRestriction, FlexibleContexts, StandaloneDeriving, BangPatterns, DeriveAnyClass, DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-orphans -Wno-name-shadowing #-}
module Main where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as GZip
import Control.Lens
import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBSC8
import Data.Char (toLower)
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.String as Str
import qualified Data.Text as T
import Language.Haskell.Exts hiding (parse)
import Language.Haskell.TH.Syntax (mkName, nameBase)
import Network.Curl.Download
import Protolude hiding (packageName, link)
import Text.HTML.Scalpel
import Text.Read
import Data.Time.Clock
import Data.Time.Calendar
import Data.List ((!!))
import Data.Text.Read
import qualified Data.Text.Encoding as T
import qualified Filesystem as FS
import qualified Filesystem.Path.CurrentOS as FScOS
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField
import Options.Applicative
import Control.DeepSeq ()
import Control.Exception (evaluate)
--import qualified Control.Monad.Parallel as ParM
--import Data.List.Split (chunksOf)
--import System.IO.Unsafe

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

data PackageInfo = PackageInfo {
  _packageName :: Text,
  _version :: Text,
  _runDate :: UTCTime,
  _dateTimeSnapshot :: UTCTime,
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
  _size :: Int64
  }
  deriving (Show,Generic)

deriving instance NFData Extension
deriving instance NFData KnownExtension
deriving instance NFData PackageInfo

makeLenses ''PackageInfo

deriving instance Generic KnownExtension
deriving instance Generic Extension

instance ToJSON KnownExtension
instance FromJSON KnownExtension
instance ToJSON Extension
instance FromJSON Extension
instance ToJSON PackageInfo where
instance FromJSON PackageInfo

scrapePackageProperties :: Text -> b -> (Text -> Scraper Text b)
  -> Scraper Text b
scrapePackageProperties cat def f = do
  res <- chroots ("table" @: ["class" @= "properties"] //
                  tagSelector "tbody" // tagSelector "tr")
                         (do
                            content <- T.strip <$> text "tr"
                            let cat':vals = fmap T.strip (T.lines content)
                            if cat' == cat then do
                              let val = T.strip (T.intercalate "\n" vals)
                              x <- f val
                              return (Just x)
                            else return Nothing)
  return (headDef def (catMaybes res))

scrapeDownloads :: Scraper Text (Int, Int)
scrapeDownloads = scrapePackageProperties "Downloads" (0,0)
                  (\val -> do
                      let totalDL = T.takeWhile (/= ' ') val
                      let thirytDaysDL = T.takeWhile (/= ' ') (T.drop 1 (T.dropWhile (/= '(') val))
                      return (read (T.unpack totalDL) :: Int,
                              read (T.unpack thirytDaysDL) :: Int))

scrapeCategories :: Scraper Text [Text]
scrapeCategories = scrapePackageProperties "Category" []
                      (\val -> do
                          return (fmap T.strip (T.splitOn "," val)))

scrapeAuthors :: Scraper Text Text
scrapeAuthors = scrapePackageProperties "Author" "None"
                  (\val -> do
                      return val)

scrapeMaintainers :: Scraper Text Text
scrapeMaintainers = scrapePackageProperties "Maintainer" "None"
                  (\val -> do
                      return val)

scrapeRating :: Scraper Text (Maybe Double)
scrapeRating = scrapePackageProperties "Rating" Nothing
                  (\val -> do
                      let er = double val
                      case er of
                        Left _ -> return Nothing
                        Right (x, _) -> return (Just x))

scrapeHomepage :: Scraper Text Text
scrapeHomepage = scrapePackageProperties "Home page" "None"
                  (\val -> do
                      return val)

scrapeBugTracker :: Scraper Text Text
scrapeBugTracker = scrapePackageProperties "Bug tracker" "None"
                  (\val -> do
                      return val)

scrapeSourceRepository :: Scraper Text Text
scrapeSourceRepository = scrapePackageProperties "Source repository" "None"
                         (\val -> do
                             return val)

scrapeUploaded :: Scraper Text UTCTime
scrapeUploaded = scrapePackageProperties "Uploaded" d0
                         (\val -> do -- Example: "Wed Dec 12 13:40:48 UTC 2012 by LennartAugustsson"
                             -- single digit days and months may result in double spaces. We have to remove these beforehand:
                             let xs = T.splitOn " " (replaceT "  " " " val)
                             let month' = xs!!1
                             let day = read (T.unpack $ xs!!2)
                             let time = xs!!3
                             let year = read (T.unpack $ xs!!5)
                             let month = if month'=="Jan" then 1
                                         else if month'=="Feb" then 2
                                         else if month'=="Mar" then 3
                                         else if month'=="Apr" then 4
                                         else if month'=="May" then 5
                                         else if month'=="Jun" then 6
                                         else if month'=="Jul" then 7
                                         else if month'=="Aug" then 8
                                         else if month'=="Sep" then 9
                                         else if month'=="Oct" then 10
                                         else if month'=="Nov" then 11
                                         else if month'=="Dec" then 12
                                         else 0
                             let ts = T.splitOn ":" time
                             let hours = read (T.unpack $ ts!!0)
                             let mins = read (T.unpack $ ts!!1)
                             let secs = read (T.unpack $ ts!!2)
                             let dt = secondsToDiffTime (hours*3600 + mins*60 + secs)
                             let d = fromGregorian year month day
                             return (UTCTime d dt))
  where d0 = (UTCTime (fromGregorian 1968 3 16) (secondsToDiffTime 0))

replaceT :: Text -> Text -> Text -> Text
replaceT old new = T.intercalate new . T.splitOn old

packageMetaData :: UTCTime -> UTCTime -> PackageShortInfo -> Scraper Text PackageInfo
packageMetaData runDate currentDateTime ps = do
  pVersions <- chroot ("table" @: ["class" @= "properties"] // tagSelector "tbody" // tagSelector "tr") (texts (tagSelector "td"))
  (dl1, dl2) <- scrapeDownloads
  auth <- scrapeAuthors
  maint <- scrapeMaintainers
  hp <- scrapeHomepage
  bt <- scrapeBugTracker
  sr <- scrapeSourceRepository
  u <- scrapeUploaded
  r <- scrapeRating
  cs <- scrapeCategories
  let vs = fmap ((\v -> if T.takeEnd 7 v==" (info)" then T.dropEnd 7 v else v)
                 . T.strip) $ T.splitOn "," (T.concat pVersions)
  return PackageInfo {
    _packageName = view sPackageName $ ps,
    _version = maximum vs,
    _runDate = runDate,
    _dateTimeSnapshot = currentDateTime,
    _tags = view sTags $ ps,
    _description = view sDescription $ ps,
    _link = view sLink $ ps,
    _categories = cs,
    _versions = vs,
    _extensionsDeclared = Set.empty,
    _totalDownloads = dl1,
    _thirtyDaysDownloads = dl2,
    _authors = auth,
    _maintainers = maint,
    _homepage = hp,
    _bugTracker = bt,
    _sourceRepository = sr,
    _uploaded = u,
    _rating = r,
    _size = 0
    }

scrapeIndividualPackageMetaData :: UTCTime -> PackageShortInfo -> IO (Maybe PackageInfo)
scrapeIndividualPackageMetaData runDate ps = do
  putStr (view sPackageName ps)
  currentDateTime <- getCurrentTime
  pm <- scrapeURL (view sLink $ ps) (packageMetaData runDate currentDateTime ps)
  case pm of
    Nothing -> do
      putStrLn (" Error while processing package "++(T.unpack $ view sPackageName ps))
      return Nothing
    Just pm' -> do
      putStr (", uploaded: "++(show (view uploaded pm')))
      epd <- analyzePackageDetails pm'
      case epd of
        Left err -> do
          putStrLn ("Error: "++err)
          return (Just pm')
        Right (exts, n) -> do
          putStrLn (", size: "++(show n)++", extensions: "++(show (length exts)))
          return (Just (pm' & extensionsDeclared .~ exts
                               & size .~ n))

analyzePackageDetails :: PackageInfo -> IO (Either Str.String (Set Extension, Int64))
analyzePackageDetails pm = do
  let pn = T.unpack . (view packageName) $ pm
  let latestPV = T.unpack . maximum . (view versions) $ pm
  let pNameVersion = pn++"-"++latestPV
  exists <- FS.isFile (FScOS.fromText (T.pack ("./packages/"++pNameVersion++".tar.gz")))
  etarGzBall <- if exists then
      Right <$> LBS.readFile ("./packages/"++pNameVersion++".tar.gz")
    else do    
      let uri = "https://hackage.haskell.org/package/"++pNameVersion++"/"++pNameVersion++".tar.gz"
      eTarGzBall <- openURI uri
      case eTarGzBall of
        Left err -> return (Left  err)
        Right tarGzBall -> do
          let tarGzBall' = LBS.fromStrict tarGzBall
          LBS.writeFile ("./packages/"++pNameVersion++".tar.gz") tarGzBall'
          return (Right tarGzBall')
  case etarGzBall of
    Left err -> return (Left err)
    Right tarGzBall -> do
      let exts = Tar.foldEntries (analyzePackageSource . Tar.entryContent) Set.empty (\_ -> Set.empty) (Tar.read (GZip.decompress tarGzBall))
      return (Right (exts, LBS.length tarGzBall))

analyzePackageSource :: Tar.EntryContent -> Set Extension -> Set Extension
analyzePackageSource (Tar.NormalFile bs _) accExtensions =
  let mExt = readExtensions (LBSC8.unpack bs)
  in case mExt of
    Just (_, exts) -> Set.union accExtensions (Set.fromList exts)
    Nothing -> accExtensions
analyzePackageSource _ accExtensions = accExtensions

instance (ToField a, ToField b, ToField c, ToField d, ToField e, ToField f,
          ToField g, ToField h, ToField i, ToField j, ToField k, ToField l, ToField m,
          ToField n, ToField o, ToField p, ToField q, ToField r, ToField s, ToField t)
    => ToRow (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t) where
    toRow (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t) =
        [toField a, toField b, toField c, toField d, toField e, toField f,
         toField g, toField h, toField i, toField j, toField k, toField l, toField m,
         toField n, toField o, toField p, toField q, toField r, toField s, toField t]

insertPackageMetaDate :: Connection -> [PackageInfo] -> IO Int64
insertPackageMetaDate conn pms = do
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
                           view description p)) pms
  executeMany conn "insert into hackage.package_snapshot (\
                   \ package_name, version, run_date, \
                   \ date_time_snapshot,\
                   \ downloads_total, downloads_30days,\
                   \ tags, authors, maintainers, \
                   \ homepage, bug_tracker, source_repository, \
                   \ uploaded, rating, size, extensions_declared, versions, \
                   \ categories, link, description)\
                   \ values (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)" values

data AppOptions = AppOptions {
  outputFileName :: FilePath,
  postgresConnectString :: Text
  }

appOptions :: Parser AppOptions
appOptions = AppOptions
  <$> strOption
  (long "outputFileName"
   <> short 'o'
   <> value "./full-hackage.json"
   <> metavar "FILE"
   <> help "File name for JSON output, default value: ./full-packages-meta-data.json")
  <*> strOption (long "postgresConnectString"
   <> short 'p'
   <> value "host=localhost port=5432 dbname=postgres connect_timeout=10"
   <> metavar "CONNECT"
   <> help "Connect string for PostgreSQL data base, default value: 'host=localhost port=5432 dbname=postgres connect_timeout=10'")

appInfo :: ParserInfo AppOptions
appInfo = info (appOptions <**> helper)
             ( fullDesc
               <> progDesc "Analyse Hackage packages and write output to a JSON file."
               <> header "Hackage Analysis Tool" )
main :: IO ()
main = do
  appOpts <- execParser appInfo
  runDate <- getCurrentTime
  xs <- fromJust <$> scrapeHackageCompleteList packageListItems
  pms <- mapM (\x -> do
                       pm <- scrapeIndividualPackageMetaData runDate x
                       evaluate (rnf pm)
                       return pm)
                   xs

  putStrLn ("Write JSON file to "++(outputFileName appOpts))
  LBS.writeFile (outputFileName appOpts) (encodePretty pms)

  putStrLn ("Connect to postgresql database 'postgres': "++(T.unpack $ postgresConnectString appOpts) :: Str.String)
  conn <- connectPostgreSQL (T.encodeUtf8 $ postgresConnectString appOpts)
  putStr ("Write to postgresql database" :: Str.String)
  res <- insertPackageMetaDate conn (catMaybes pms)
  putStrLn (", result: "++show res)
