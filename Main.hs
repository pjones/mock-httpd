-- |
--
-- Copyright:
--   This file is part of the package mock-httpd. It is subject to the
--   license terms in the LICENSE file found in the top-level
--   directory of this distribution and at:
--
--     https://github.com/pjones/mock-httpd
--
--   No part of this package, including this file, may be copied,
--   modified, propagated, or distributed except according to the terms
--   contained in the LICENSE file.
--
-- License: BSD-2-Clause
module Main
  ( main,
  )
where

import qualified Control.Concurrent.Async as Async
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as ByteString
import qualified Data.CaseInsensitive as CaseInsensitive
import qualified Data.Text as Text
import qualified Data.Yaml as YML
import GHC.Generics (Rep)
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import Relude.Extra.Map
import System.Environment (getArgs)
import System.FilePath ((</>))

-- | Listen on an HTTP port with specific settings.
--
-- @since 0.0.0.0
newtype Listen = Listen
  { listenPort :: Int
  }
  deriving stock (Generic, Show)
  deriving (Aeson.ToJSON, Aeson.FromJSON) via GenericJSON Listen

-- | Create/modify a response using various actions.
--
-- @since 0.0.0.0
data Action
  = ServeFile FilePath
  | ServeText Text
  | Location Text
  | ContentType Text
  | SetHeaders (HashMap Text Text)
  | ResponseCode Int
  | SaveBody FilePath
  deriving stock (Generic, Show)
  deriving (Aeson.ToJSON, Aeson.FromJSON) via GenericJSON Action

-- | Match a request and run associated actions to generate a
-- response.
--
-- @since 0.0.0.0
data Route = Route
  { routeMatchPath :: Maybe Text,
    routeMatchMethod :: Maybe Text,
    routeActions :: NonEmpty Action
  }
  deriving stock (Generic, Show)
  deriving (Aeson.ToJSON, Aeson.FromJSON) via GenericJSON Route

-- | Configuration.
--
-- @since 0.0.0.0
data Config = Config
  { configListen :: NonEmpty Listen,
    configDirectory :: FilePath,
    configRoutes :: NonEmpty Route
  }
  deriving stock (Generic, Show)
  deriving (Aeson.ToJSON, Aeson.FromJSON) via GenericJSON Config

-- | Type wrapper for automatic JSON deriving.
newtype GenericJSON a = GenericJSON
  {genericJSON :: a}

-- | Default JSON decoding/encoding options.
aesonOptions :: Aeson.Options
aesonOptions =
  Aeson.defaultOptions
    { Aeson.fieldLabelModifier = snakeCase >>> dropFirstWord,
      Aeson.constructorTagModifier = snakeCase,
      Aeson.allNullaryToStringTag = True,
      Aeson.omitNothingFields = True,
      Aeson.sumEncoding = Aeson.ObjectWithSingleField
    }
  where
    dropFirstWord = dropWhile (/= '_') >>> drop 1
    snakeCase = Aeson.camelTo2 '_'

instance
  ( Generic a,
    Aeson.GToJSON Aeson.Zero (Rep a),
    Aeson.GToEncoding Aeson.Zero (Rep a)
  ) =>
  Aeson.ToJSON (GenericJSON a)
  where
  toJSON = Aeson.genericToJSON aesonOptions . genericJSON
  toEncoding = Aeson.genericToEncoding aesonOptions . genericJSON

instance
  ( Generic a,
    Aeson.GFromJSON Aeson.Zero (Rep a)
  ) =>
  Aeson.FromJSON (GenericJSON a)
  where
  parseJSON = fmap GenericJSON . Aeson.genericParseJSON aesonOptions

-- | The types of responses that can be generated from an action.
--
-- @since 0.0.0.0
data Response m
  = Create Wai.Response
  | Modify (Wai.Response -> Wai.Response)
  | Effect (m ())

-- | Create a response from a request and 'Action'.
--
-- @since 0.0.0.0
respond :: MonadIO m => Config -> Wai.Request -> Action -> Response m
respond cfg req = \case
  ServeFile path ->
    let file = configDirectory cfg </> path
     in Create (Wai.responseFile HTTP.status200 mempty file Nothing)
  ServeText text ->
    Create (Wai.responseLBS HTTP.status200 mempty (encodeUtf8 text))
  Location url ->
    Modify $
      Wai.mapResponseHeaders $ \headers ->
        ("Location", encodeUtf8 url) : headers
  ContentType value ->
    Modify $
      Wai.mapResponseHeaders $ \headers ->
        ("Content-Type", encodeUtf8 value) : headers
  SetHeaders hmap ->
    Modify $
      Wai.mapResponseHeaders
        ( <>
            ( toPairs hmap
                <&> bimap
                  (encodeUtf8 >>> CaseInsensitive.mk)
                  encodeUtf8
            )
        )
  ResponseCode code ->
    Modify $
      Wai.mapResponseStatus $
        const (HTTP.mkStatus code (show code))
  SaveBody path ->
    Effect $
      liftIO $
        withFile (configDirectory cfg </> path) WriteMode $ \h ->
          let go = do
                bs <- Wai.getRequestBodyChunk req
                if ByteString.null bs
                  then pure ()
                  else ByteString.hPut h bs >> go
           in go

-- | Basic information about a request for matching routes.
--
-- @since 0.0.0.0
data Request = Request
  { requestPath :: Text,
    requestMethod :: Text
  }
  deriving stock (Generic, Show)
  deriving (Aeson.ToJSON, Aeson.FromJSON) via GenericJSON Request

-- | FIXME: Write description for toRequest
--
-- @since 0.0.0.0
toRequest :: Wai.Request -> Request
toRequest req =
  Request
    { requestPath = "/" <> Text.intercalate "/" (Wai.pathInfo req),
      requestMethod = decodeUtf8 (Wai.requestMethod req)
    }

-- | Does a 'Route' match a request?
--
-- @since 0.0.0.0
match :: Request -> Route -> Bool
match Request {..} Route {..} =
  all
    (fromMaybe True)
    [ matchPath,
      matchMethod
    ]
  where
    matchPath :: Maybe Bool
    matchPath = do
      path <- routeMatchPath
      pure (requestPath == path)
    matchMethod :: Maybe Bool
    matchMethod = do
      method <- routeMatchMethod
      pure (requestMethod == method)

-- | Start a single web server.
--
-- @since 0.0.0.0
server :: Config -> Listen -> IO ()
server cfg@Config {..} listen@Listen {..} = do
  putLBSLn (Aeson.encode listen)
  Warp.run listenPort go
  where
    go :: Wai.Application
    go req resf = do
      let req' = toRequest req
      res <- case toList configRoutes & filter (match req') of
        [] -> do
          putLBSLn (Aeson.encode (req', ResponseCode 404))
          pure (Wai.responseLBS HTTP.status404 mempty mempty)
        r : _ -> do
          putLBSLn (Aeson.encode (req', r))
          route r req
      resf res
    route :: Route -> Wai.Request -> IO Wai.Response
    route Route {..} req = do
      let actions = fmap (respond cfg req) routeActions
          response = listToMaybe [r | Create r <- toList actions]
      foldlM
        ( \res action ->
            case action of
              Create _ -> pure res -- Already handled
              Modify f -> pure (f res)
              Effect k -> k >> pure res
        )
        (fromMaybe (Wai.responseLBS HTTP.status200 mempty mempty) response)
        actions

-- | Main.
main :: IO ()
main = do
  args <- getArgs
  cfg <- case args of
    [path] -> YML.decodeFileThrow path
    _ -> die "Usage: mockhttp <configuration-file>"
  Async.mapConcurrently_ (server cfg) (configListen cfg)
