{-# OPTIONS_GHC -Wno-name-shadowing -Wno-unused-do-bind #-}
module Hackage.Scrape.PackageDetails where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as GZip
import           Control.Applicative
import           Control.Lens hiding ((<.>))
import           Data.Attoparsec.Text hiding (take, double)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBSC8
import           Data.Char (isAlpha, isDigit)
import           Data.List ((!!))
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.String as Str
import qualified Data.Text as T
import           Data.Text.Read (double)
import           Data.Time.Calendar
import           Data.Time.Clock
import           Language.Haskell.Exts hiding (parse)
import           Protolude hiding (packageName, link, takeWhile, option, (<.>))
import           Text.HTML.Scalpel
import           Text.Read (read)

import Hackage.Scrape.PackageInfo
import Hackage.Scrape.PackageArchive
import Hackage.Scrape.CompleteList

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

scrapeDependencies :: Scraper Text (Either Text [(Text, Maybe Text)])
scrapeDependencies =
  scrapePackageProperties "Dependencies" (Right [])
  (\val -> do
      let res = parseOnly dependenciesParser val
      return $ case res of
                 Left err -> Left $ T.concat ["Error in scrapeDependencies: ",
                                               T.pack err]
                 Right x -> Right x)

-- e.g. 'base (>=3 && <5), flexible-defaults (>=0.0.0.2), mersenne-random-pure64, mtl (>=1 && <3), mwc-random, random, stateref (==0.3.*), syb, template-haskell, th-extras [details]'
dependencyParser :: Parser (Text, Maybe Text)
dependencyParser = do
          c1 <- T.singleton <$> letter
          pkgWithoutC1 <- takeWhile1 (\c -> isAlpha c || isDigit c || c=='-')
          let pkg = T.concat [c1, pkgWithoutC1]
          skipSpace
          mVersion <- choice [
            do
              char '('
              versionSpec <- takeWhile1 (/=')')
              string ")"
              option ' ' (char ',')
              skipSpace
              return (Just versionSpec),
            do
              string ","
              skipSpace
              return Nothing,
            do
              string "["
              takeWhile (\_ -> True)
              return Nothing
            ]
          return (pkg, mVersion)

dependenciesParser :: Parser [(Text, Maybe Text)]
dependenciesParser = many dependencyParser

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

packageMetaData :: UTCTime -> UTCTime -> PackageShortInfo
  -> Scraper Text ([Text], PackageInfo)
packageMetaData runDate currentDateTime ps = do
  pVersions <- chroot ("table" @: ["class" @= "properties"]
                       // tagSelector "tbody"
                       // tagSelector "tr") (texts (tagSelector "td"))
  (dl1, dl2) <- scrapeDownloads
  auth <- scrapeAuthors
  maint <- scrapeMaintainers
  hp <- scrapeHomepage
  bt <- scrapeBugTracker
  sr <- scrapeSourceRepository
  u <- scrapeUploaded
  r <- scrapeRating
  cs <- scrapeCategories
  eDeps <- scrapeDependencies
  let (msgDeps, deps) = case eDeps of
        Left msg -> ([msg], [])
        Right deps -> ([], deps)
  let vs = fmap ((\v -> if T.takeEnd 7 v==" (info)" then T.dropEnd 7 v else v)
                 . T.strip) $ T.splitOn "," (T.concat pVersions)
  return (msgDeps,
          PackageInfo {
             _packageName = ps ^. sPackageName,
             _version = maximum vs,
             _runDate = runDate,
             _dateTimeSnapshot = currentDateTime,
             _dependencies = deps,
             _tags = ps ^. sTags,
             _description = ps ^. sDescription,
             _link = ps ^. sLink,
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
             _size = 0,
             _packageBuildStatus = Nothing
             })

scrapeIndividualPackageMetaData :: UTCTime -> PackageShortInfo
  -> IO (Maybe PackageInfo)
scrapeIndividualPackageMetaData runDate ps = do
  currentDateTime <- getCurrentTime
  pm <- scrapeURLWithRetry 10 (ps ^. sLink) (packageMetaData runDate currentDateTime ps)
  mPI <- case pm of
           Nothing -> do
             return Nothing
           Just (msgs, pm') -> do
             mapM_ (\m -> putStrLn (T.unpack m)) msgs
             epd <- analyzePackageDetails pm'
             case epd of
               Left err -> do
                 putStrLn ("\n***Error: "++err)
                 return (Just pm')
               Right (exts, n) -> do
                 return (Just (pm' & extensionsDeclared .~ exts & size .~ n))
  return mPI

scrapeURLWithRetry :: Int -> URL -> Scraper Text a -> IO (Maybe a)
scrapeURLWithRetry n link scraper = do
  x <- scrapeURL link scraper
  case x of
    Nothing -> do
      if n>0 then do
        putStrLn ("\n***Scraping URL failed. Wait 10 s and try again.***\n" :: Text)
        let tenSeconds = 10000000  -- in µs
        threadDelay tenSeconds
        scrapeURLWithRetry (n-1) link scraper
      else return Nothing
    Just _ -> return x

analyzePackageDetails :: PackageInfo -> IO (Either Str.String (Set Extension, Int64))
analyzePackageDetails pm = do
  etarGzBall <- getPackageArchive (T.unpack (pm^.packageName))
                                  (T.unpack (maximum (pm^.versions)))
  case etarGzBall of
    Left err -> return (Left err)
    Right tarGzBall -> do
      let decompressed = Tar.read (GZip.decompress tarGzBall)
      let exts = Tar.foldEntries (analyzePackageSource . Tar.entryContent)
                                 Set.empty (const Set.empty)
                                 decompressed
      return (Right (exts, LBS.length tarGzBall))

analyzePackageSource :: Tar.EntryContent -> Set Extension -> Set Extension
analyzePackageSource (Tar.NormalFile bs _) accExtensions =
  let mExt = readExtensions (LBSC8.unpack bs)
  in case mExt of
    Just (_, exts) -> Set.union accExtensions (Set.fromList exts)
    Nothing -> accExtensions
analyzePackageSource _ accExtensions = accExtensions
