module Hackage.Scrape.PackageArchive where

import           Control.Applicative
import           Control.DeepSeq ()
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBSC8
import qualified Data.String as Str
import           System.FilePath.Posix ((</>), (<.>))
import           System.Directory (doesFileExist, getHomeDirectory)
import           Network.Curl.Download
import           Protolude hiding (packageName, link, takeWhile, option, (<.>))

packageTarGzUri :: Str.String -> Str.String
packageTarGzUri pNameVersion = "https://hackage.haskell.org/package/"++
                                   pNameVersion++"/"++pNameVersion++".tar.gz"

getPackageArchive :: Str.String -> Str.String -> IO (Either Str.String LBSC8.ByteString)
getPackageArchive pn latestPV = do
  let pNameVersion = pn++"-"++latestPV
  userRoot <- getHomeDirectory
  let packagesRoot = userRoot </> ".hackage-analyzer-packages"
      packageTarGz = packagesRoot </> pNameVersion <.> ".tar.gz"
  exists <- doesFileExist packageTarGz
  if exists then
      Right <$> LBS.readFile packageTarGz
    else do    
      let uri = packageTarGzUri pNameVersion
      eTarGzBall <- openURI uri
      case eTarGzBall of
        Left err -> return (Left  err)
        Right tarGzBall -> do
          let tarGzBall' = LBS.fromStrict tarGzBall
          LBS.writeFile packageTarGz tarGzBall'
          return (Right tarGzBall')
