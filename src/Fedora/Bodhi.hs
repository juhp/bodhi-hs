{-# LANGUAGE CPP #-}

{- |
Copyright: (c) 2020 Jens Petersen
SPDX-License-Identifier: MIT
Maintainer: Jens Petersen <juhpetersen@gmail.com>

Fedora Bodhi REST client library
-}

module Fedora.Bodhi
  ( bodhiBuild
  , bodhiBuilds
  , bodhiComment
  , bodhiComments
  , bodhiCSRF
  , bodhiOverride
  , bodhiOverrides
  , bodhiOverrideDates
  , bodhiPackages
  , bodhiRelease
  , bodhiReleases
  , bodhiUpdate
  , bodhiUpdates
  , bodhiUser
  , bodhiUsers
  , lookupKey
  , lookupKey'
  , queryBodhi
  , makeKey
  , makeItem
  , maybeKey
  , Query
  , QueryItem
  ) where

#if (defined(VERSION_lens_aeson))
import Control.Lens
import Data.Aeson.Lens
#else
import Lens.Micro
import Lens.Micro.Aeson
#endif
#if (defined(VERSION_aeson_pretty))
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as BL
import Network.HTTP.Conduit (queryString)
#endif
import Data.Aeson.Types
#if (defined(MIN_VERSION_http_conduit) && MIN_VERSION_http_conduit(2,3,3))
#else
import Data.ByteString (ByteString)
#endif
import qualified Data.ByteString.Char8 as B
import Data.Maybe
import Data.Text (Text)
import Data.Time.LocalTime
import Network.HTTP.Simple
import System.FilePath ((</>))

server :: String
server = "bodhi.fedoraproject.org"

-- | Returns build JSON for NVR
--
-- https://bodhi.fedoraproject.org/docs/server_api/rest/builds.html#service-0
bodhiBuild :: String -> IO Object
bodhiBuild nvr = do
  res <- queryBodhi [] $ "builds" </> nvr
  return $ res ^. _Object

-- | returns JSON list of builds
--
-- https://bodhi.fedoraproject.org/docs/server_api/rest/builds.html#service-1
bodhiBuilds :: Query -> IO [Object]
bodhiBuilds params = do
  res <- queryBodhi params "builds/"
  return $ res ^.. key "builds" . values . _Object

-- | Returns comment JSON for id
--
-- https://bodhi.fedoraproject.org/docs/server_api/rest/comments.html#service-0
bodhiComment :: String -> IO Object
bodhiComment cid = do
  res <- queryBodhi [] $ "comments" </> cid
  return $ res ^. _Object

-- | returns JSON list of comments
--
-- https://bodhi.fedoraproject.org/docs/server_api/rest/comments.html#service-1
bodhiComments :: Query -> IO [Object]
bodhiComments params = do
  res <- queryBodhi params "comments/"
  return $ res ^.. key "comments" . values . _Object

-- | Get CSRF token
--
-- https://bodhi.fedoraproject.org/docs/server_api/rest/csrf.html
bodhiCSRF :: IO (Maybe Text)
bodhiCSRF = do
  res <- queryBodhi [] "csrf"
  return $ res ^? key "csrf_token" . _String

-- | Returns override JSON for NVR
--
-- https://bodhi.fedoraproject.org/docs/server_api/rest/overrides.html#service-0
bodhiOverride :: String -> IO (Maybe Object)
bodhiOverride nvr = do
  res <- queryBodhi [] $ "overrides" </> nvr
  return $ res ^? key "override" . _Object

#if (defined(MIN_VERSION_http_conduit) && MIN_VERSION_http_conduit(2,3,1))
#else
type Query = [(ByteString, Maybe ByteString)]
#endif
#if (defined(MIN_VERSION_http_conduit) && MIN_VERSION_http_conduit(2,3,3))
#else
type QueryItem = (ByteString, Maybe ByteString)
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
  res <- queryBodhi params "overrides/"
  return $ res ^.. key "overrides" . values . _Object

-- | Packages query
--
-- https://bodhi.fedoraproject.org/docs/server_api/rest/packages.html#service-0
bodhiPackages :: Query -> IO [Object]
bodhiPackages params = do
  res <- queryBodhi params "packages/"
  return $ res ^.. key "packages" . values . _Object

-- | read releases metadata from Bodhi
--
-- https://bodhi.fedoraproject.org/docs/server_api/rest/releases.html#service-0
bodhiRelease :: String -> IO Object
bodhiRelease rel = do
  res <- queryBodhi [] $ "releases" </> rel
  return $ res ^. _Object

-- | read releases metadata from Bodhi
--
-- https://bodhi.fedoraproject.org/docs/server_api/rest/releases.html#service-1
bodhiReleases :: Query -> IO [Object]
bodhiReleases params = do
  res <- queryBodhi params "releases/"
  return $ res ^.. key "releases" . values . _Object

-- | read an update from Bodhi
--
-- https://bodhi.fedoraproject.org/docs/server_api/rest/updates.html#service-0
bodhiUpdate :: String -> IO (Maybe Object)
bodhiUpdate update = do
  res <- queryBodhi [] $ "updates" </> update
  return $ res ^? key "update" . _Object

-- | search for updates on Bodhi
--
-- https://bodhi.fedoraproject.org/docs/server_api/rest/updates.html#service-2
bodhiUpdates :: Query -> IO [Object]
bodhiUpdates params = do
  res <- queryBodhi params "updates/"
  return $ res ^.. key "updates" . values . _Object

-- | user info from Bodhi
--
-- https://bodhi.fedoraproject.org/docs/server_api/rest/users.html#service-0
bodhiUser :: String -> IO Object
bodhiUser user = do
  res <- queryBodhi [] $ "users" </> user
  return $ res ^. _Object

-- | list users from Bodhi
--
-- https://bodhi.fedoraproject.org/docs/server_api/rest/users.html#service-1
bodhiUsers :: Query -> IO [Object]
bodhiUsers params = do
  res <- queryBodhi params "users/"
  return $ res ^.. key "users" . values . _Object

-- | low-level query
queryBodhi :: Query -> String -> IO Value
queryBodhi params path = do
  let url = "https://" ++ server </> path
  req <- setRequestQueryString params <$> parseRequest url
#if (defined(VERSION_aeson_pretty))
  putStrLn $ url ++ B.unpack (queryString req)
  res <- getResponseBody <$> httpJSON req
  BL.putStrLn $ encodePretty res
  return res
#else
  getResponseBody <$> httpJSON req
#endif

-- | Maybe create a query key
maybeKey :: String -> Maybe String -> Query
maybeKey _ Nothing = []
maybeKey k mval = [(B.pack k, fmap B.pack mval)]

-- | make a singleton key-value Query
makeKey :: String -> String -> Query
makeKey k val = [(B.pack k, Just (B.pack val))]

-- | make a key-value QueryItem
makeItem :: String -> String -> QueryItem
makeItem k val = (B.pack k, Just (B.pack val))

-- | looks up key in object
lookupKey :: FromJSON a => Text -> Object -> Maybe a
lookupKey k = parseMaybe (.: k)

-- | like lookupKey but raises an error if no key found
lookupKey' :: FromJSON a => Text -> Object -> a
lookupKey' k obj =
  fromMaybe (error ("no key: " ++ show k)) (lookupKey k obj)
