{-# LANGUAGE CPP #-}

{- |
Copyright: (c) 2020 Jens Petersen
SPDX-License-Identifier: MIT
Maintainer: Jens Petersen <juhpetersen@gmail.com>

Fedora Bodhi REST client library
-}

module Web.Fedora.Bodhi
  ( bodhiOverride
  , bodhiOverrides
  , bodhiOverrideDates
  , bodhiRelease
  , bodhiReleases
  , bodhiUpdate
  , bodhiUpdates
  , lookupKey
  , lookupKey'
  , queryBodhi
  , makeKey
  , maybeKey
  ) where

import Control.Monad
#if (defined(VERSION_lens_aeson))
import Control.Lens
import Data.Aeson.Lens
#else
import Lens.Micro
import Lens.Micro.Aeson
#endif
import Data.Aeson.Encode.Pretty
import Data.Aeson.Types
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Maybe
import Data.Text (Text)
import Data.Time.LocalTime
import Network.HTTP.Conduit (queryString)
import Network.HTTP.Simple
import System.FilePath ((</>))

server :: String
server = "bodhi.fedoraproject.org"

-- https://bodhi.fedoraproject.org/docs/server_api/rest/overrides.html

-- | Returns override JSON for NVR
--
-- https://bodhi.fedoraproject.org/docs/server_api/rest/overrides.html#service-0
bodhiOverride :: String -> IO (Maybe Object)
bodhiOverride nvr = do
  res <- queryBodhi False [] $ "overrides" </> nvr
  return $ res ^? key "override" . _Object

#if (defined(MIN_VERSION_http_conduit) && MIN_VERSION_http_conduit(2,3,1))
#else
type Query = [(ByteString, Maybe ByteString)]
#endif

-- | Returns override expiration and submission dates for NVR
bodhiOverrideDates :: String -> IO (Maybe (LocalTime,LocalTime))
bodhiOverrideDates nvr = do
  mobj <- bodhiOverride nvr
  case mobj of
    Nothing -> do
      putStrLn $ "Override for " ++ nvr ++ " not found"
      return Nothing
    Just obj -> return $ readDates obj
  where
    readDates :: Object -> Maybe (LocalTime,LocalTime)
    readDates =
      parseMaybe $ \obj -> do
        expire <- obj .: "expiration_date"
        submit <- obj .: "submission_date"
        return (expire,submit)

-- | returns JSON list of overrides
--
-- https://bodhi.fedoraproject.org/docs/server_api/rest/overrides.html#service-1
bodhiOverrides :: Query -> IO [Object]
bodhiOverrides params = do
  res <- queryBodhi False params "overrides/"
  return $ res ^.. key "overrides" . values . _Object

-- | read releases metadata from Bodhi
--
-- https://bodhi.fedoraproject.org/docs/server_api/rest/releases.html#service-0
bodhiRelease :: String -> IO (Maybe Object)
bodhiRelease rel = do
  res <- queryBodhi False [] $ "releases" </> rel
  return $ res ^? _Object

-- | read releases metadata from Bodhi
--
-- https://bodhi.fedoraproject.org/docs/server_api/rest/releases.html#service-1
bodhiReleases :: Query -> IO [Object]
bodhiReleases params = do
  res <- queryBodhi False params "releases/"
  return $ res ^.. key "releases" . values . _Object

-- | read releases metadata from Bodhi
--
-- https://bodhi.fedoraproject.org/docs/server_api/rest/updates.html#service-0
bodhiUpdate :: String -> IO (Maybe Object)
bodhiUpdate update = do
  res <- queryBodhi False [] $ "updates" </> update
  return $ res ^? key "update" . _Object

-- | read releases metadata from Bodhi
--
-- https://bodhi.fedoraproject.org/docs/server_api/rest/updates.html#service-2
bodhiUpdates :: Query -> IO [Object]
bodhiUpdates params = do
  res <- queryBodhi False params "updates/"
  return $ res ^.. key "updates" . values . _Object

-- | low-level query
queryBodhi :: Bool -> Query -> String -> IO Value
queryBodhi debug params path = do
  let url = "https://" <> server </> path
  req <- setRequestQueryString params <$> parseRequest url
  putStrLn $ url ++ B.unpack (queryString req)
  res <- getResponseBody <$> httpJSON req
  when debug $
    BL.putStrLn $ encodePretty res
  return res

-- | Maybe create a query key
maybeKey :: String -> Maybe String -> Query
maybeKey _ Nothing = []
maybeKey k mval = [(B.pack k, fmap B.pack mval)]

-- | make a query key
makeKey :: String -> String -> Query
makeKey k val = [(B.pack k, Just (B.pack val))]

-- | looks up key in object
lookupKey :: FromJSON a => Text -> Object -> Maybe a
lookupKey k = parseMaybe (.: k)

-- | like lookupKey but raises an error if no key found
lookupKey' :: FromJSON a => Text -> Object -> a
lookupKey' k obj =
  fromMaybe (error ("no key: " ++ show k)) (lookupKey k obj)
