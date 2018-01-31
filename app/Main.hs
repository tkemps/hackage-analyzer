{-# OPTIONS_GHC -fno-warn-orphans -Wno-name-shadowing -Wno-unused-do-bind #-}
module Main where

import           Control.Applicative
import           Control.DeepSeq ()
import           Control.Lens
import           Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy as LBS
import           Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time.Clock
import           Database.PostgreSQL.Simple
import qualified Options.Applicative as OptA
import           Protolude hiding (packageName, link, takeWhile, option)
import           Test.WebDriver (runSession, useBrowser, defaultConfig,
                                 finallyClose, chrome)

import Hackage.Scrape.CompleteList
import Hackage.Scrape.PackageDetails
import Hackage.Scrape.BuildStatus
import Hackage.Scrape.PackageInfo
import Hackage.Scrape.Db

data AppOptions = AppOptions {
  outputFileName :: FilePath,
  postgresConnectString :: Text,
  packages :: [Text],
  delete :: Bool,
  verbose :: Bool
  } deriving Show
 
appOptions :: OptA.Parser AppOptions
appOptions = AppOptions
  <$> OptA.strOption (OptA.long "outputFileName"
   <> OptA.short 'o'
   <> OptA.value ""
   <> OptA.metavar "FILE"
   <> OptA.help "File name for JSON output, default is not to write a file")
  <*> OptA.strOption (OptA.long "postgresConnectString"
   <> OptA.short 'p'
   <> OptA.value "host=localhost port=5432 dbname=postgres connect_timeout=10"
   <> OptA.metavar "CONNECT"
   <>  OptA.help "Connect string for PostgreSQL data base, default value: 'host=localhost port=5432 dbname=postgres connect_timeout=10'")
  <*> many (OptA.strOption (OptA.long "packages"
   <> OptA.metavar "PACKAGE_NAME"
   <>  OptA.help "Name of a package to be processed"))
  <*> OptA.switch (OptA.long "delete"
    <> OptA.short 'd'
    <> OptA.help "Whether to delete the data for this run_date before inserting new data" )
  <*> OptA.switch (OptA.long "verbose"
    <> OptA.short 'v'
    <> OptA.help "Whether to be verbose" )

appInfo :: OptA.ParserInfo AppOptions
appInfo = OptA.info (appOptions <**> OptA.helper)
             ( OptA.fullDesc
               <> OptA.progDesc "Analyse Hackage packages and write output to a JSON file."
               <> OptA.header "Hackage Analysis Tool" )

main :: IO ()
main = do
  appOpts <- OptA.execParser appInfo
  let vPutStrLn :: Text -> IO ()
      vPutStrLn s = when (verbose appOpts) $ putStrLn s
      vPutStr :: Text -> IO ()
      vPutStr s = when (verbose appOpts) $ putStr s
  vPutStrLn (T.concat ["Program options: ", show appOpts])
  runDate <- getCurrentTime
  xs' <- fromJust <$> scrapeHackageCompleteList packageListItems
  putStrLn ("Found "++show (length xs')++" packages.")
  let xs = if null (packages appOpts)
            then xs'
            else filter (\x -> view sPackageName x `elem` packages appOpts) xs'
  when (not (null (packages appOpts))) $ do
    vPutStrLn (T.concat ["Reduced to ",
                         let l = length xs
                             pTerm = if l>1
                                     then " packages."
                                     else " package."
                         in T.concat [show l, pTerm]])
  pms <- bracket
    (do
        vPutStrLn (T.concat ["Connect to postgresql database: ",
                             postgresConnectString appOpts])
        connectPostgreSQL (T.encodeUtf8 $ postgresConnectString appOpts))
    (\conn -> do
        close conn
        vPutStrLn ("Data base connection closed." :: Text))
    (\conn -> do
        -- the following runs in the WD monad which implements MonadIO. The function
        -- scrapeBuildStatus needs webdriver. Therefore we have to liftIO all the
        -- IO actions.
        when (delete appOpts) (
          do
            putStrLn ("Delete data for current run date "++show runDate++".")
            deleteRunDateFromDb conn runDate
            return ()
          )
        let cfg = useBrowser chrome defaultConfig
        runSession cfg . finallyClose $ do
          mapM (\x -> do
                   liftIO $ putStrLn T.empty
                   liftIO $ putStr (x ^. sPackageName)
                   mPM <- liftIO $ scrapeIndividualPackageMetaData runDate x
                   mBS <- case mPM of
                     Just pm -> do
                       liftIO $ vPutStr (T.concat [
                                            ", uploaded: ",
                                            show (pm ^. uploaded),
                                            ", dependencies: ",
                                            show (length (pm ^. dependencies))])
                       liftIO $ insertPackageMetaData conn pm
                       mBS <- scrapeBuildStatus (x ^. sPackageName)
                       liftIO $ vPutStrLn (T.concat ["Build status: ", show mBS])
                       case mBS of
                         Just bs -> liftIO $ insertBuildStatus conn pm bs
                         Nothing -> return 0
                       return mBS
                     Nothing -> do
                       liftIO $ putStr ("\n***Error while processing package "
                                        ++(T.unpack $ x ^. sPackageName))
                       return Nothing
                   return (mPM & _Just . packageBuildStatus .~ mBS))
            xs)
  when (not (null (outputFileName appOpts))) $ do
    putStrLn ("\n\nWrite JSON file to "++(outputFileName appOpts))
    LBS.writeFile (outputFileName appOpts) (encodePretty pms)
