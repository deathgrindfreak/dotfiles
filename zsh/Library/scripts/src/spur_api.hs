{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (void)
import Data.Maybe (catMaybes)
import qualified Data.List as L
import Turtle hiding (env)

data KPSEnv = Prod | PreProd | UAT | Cert | Dev | Local | FunctionalTest deriving (Show)
data APIType = Public | Private | Integration deriving (Show)
newtype Route = Route {routeToText :: Text} deriving (Show)
newtype HTTPVerb = HTTPVerb {verbToText :: Text} deriving (Show)

textToEnv :: Text -> Either Text KPSEnv
textToEnv envTxt =
  case envTxt of
    "prod" -> Right Prod
    "preprod" -> Right PreProd
    "uat" -> Right UAT
    "cert" -> Right Cert
    "dev" -> Right Dev
    "local" -> Right Local
    "functional_test" -> Right FunctionalTest
    _ ->
      Left $
        "Unknown environment: \""
          <> envTxt
          <> "\". Must be one of prod, preprod, uat, cert, dev, local or functional_test"

textToApiType :: Text -> Either Text APIType
textToApiType apiTxt =
  case apiTxt of
    "public" -> Right Public
    "private" -> Right Private
    "integration" -> Right Integration
    _ ->
      Left $
        "Unknown API type: \""
          <> apiTxt
          <> "\". Must be one of public, private or integration"

textToVerb :: Maybe Text -> Either Text HTTPVerb
textToVerb = maybe (Right (HTTPVerb "GET")) toVerb
  where
    toVerb v =
      case v of
        "GET" -> Right $ HTTPVerb "GET"
        "POST" -> Right $ HTTPVerb "POST"
        "HEAD" -> Right $ HTTPVerb "HEAD"
        "PUT" -> Right $ HTTPVerb "PUT"
        "DELETE" -> Right $ HTTPVerb "DELETE"
        _ -> Left $ "Unknown HTTP verb: " <> v

-- Pretty broad assumption, but we don't handle a ton of different mime types
textToAccept :: Maybe Text -> Either Text Text
textToAccept = maybe (Right "application/json") toAccept
  where
    toAccept v =
      case v of
        "json" -> Right "application/json"
        "xml" -> Right "application/xml"
        "text" -> Right "text/plain;charset=utf-8"
        _ -> Left $ "Unknown Accept header type: " <> v

-- Pretty broad assumption, but we don't handle a ton of different mime types
textToContentType :: Maybe Text -> Either Text (Maybe Text)
textToContentType v =
  case v of
    Just "json" -> Right (Just "application/json")
    Just "xml" -> Right (Just "application/xml")
    Just "text" -> Right (Just "text/plain;charset=utf-8")
    Just v -> Left $ "Unknown Content Type header: " <> v
    Nothing -> Right Nothing

parseOrDie :: b -> (b -> Either Text a) -> IO a
parseOrDie e f =
  case f e of
    Right env -> pure env
    Left err -> die err

optParser :: Parser (Text, Text, Text, Maybe Text, Maybe Text, Maybe Text, Maybe Text, Bool)
optParser =
  (,,,,,,,)
    <$> argText "route" "The relative API path"
    <*> optText "env" 'e' "The KPS environment (prod, preprod, uat, cert, dev, local or functional_test)"
    <*> optText "api" 'a' "The API type (public, private or integration)"
    <*> optional (optText "request" 'X' "Request type (http verb)")
    <*> optional (optText "accept" 't' "Accept type for the request (json, xml or text)")
    <*> optional (optText "content-type" 'c' "Content type for the request (json)")
    <*> optional (optText "data" 'd' "Data to pass to curl during a POST")
    <*> switch "verbose" 'v' "Verbose mode for curl"

data APIOptions = APIOptions
  { env :: KPSEnv
  , api :: APIType
  , route :: Route
  , verb :: HTTPVerb
  , accept :: Text
  , contentType :: Maybe Text
  , spurToken :: Text
  , apiKey :: Text
  , dataBody :: Maybe Text
  , verbose :: Bool
  }
  deriving (Show)

parseOptions :: IO APIOptions
parseOptions = do
  (route, envTxt, apiTxt, verbTxt, acceptTxt, contentTypeTxt, dataBody, verbose) <-
    options "A wrapper for SPUR APIs" optParser
  env <- parseOrDie envTxt textToEnv
  api <- parseOrDie apiTxt textToApiType
  verb <- parseOrDie verbTxt textToVerb
  accept <- parseOrDie acceptTxt textToAccept
  contentType <- parseOrDie contentTypeTxt textToContentType

  spurToken <- lookupOrDie (spurTokenName env api)
  apiKey <- lookupOrDie (apiKeyName env)
  return $ APIOptions env api (Route route) verb accept contentType spurToken apiKey dataBody verbose
  where
    lookupOrDie vName = do
      v <- need vName
      case v of
        Nothing -> die $ "Could not find variable \"" <> vName <> "\""
        Just ev -> pure ev

    spurTokenName e a =
      let eString =
            case e of
              Prod -> "PROD"
              PreProd -> "PREPROD"
              UAT -> "UAT"
              Cert -> "CERT"
              Dev -> "DEV"
              Local -> "LOCAL"
              FunctionalTest -> "LOCAL"
          aString =
            case a of
              Private -> "PRIVATE"
              Public -> "PUBLIC"
              Integration -> "INTEGRATION"
       in mconcat $ L.intersperse "_" ["KPS", eString, aString]

    apiKeyName e =
      let eString =
            case e of
              Prod -> "PROD"
              PreProd -> "PREPROD"
              UAT -> "UAT"
              Cert -> "CERT"
              Dev -> "DEV"
              Local -> "LOCAL"
              FunctionalTest -> "LOCAL"
       in "KPS_" <> eString <> "_API_TOKEN"

constructURL :: APIOptions -> Text
constructURL APIOptions { env, api, route = Route route } =
  mconcat (L.intersperse "/" $ catMaybes [Just (url env), baseRoute env]) <> route
  where
    url e =
      case e of
        Prod -> "https://coreapi.heb.com"
        PreProd -> "https://coreapi.heb.com"
        UAT -> "https://coreapi.uat.heb.com"
        Cert -> "https://coreapi.uat.heb.com"
        Dev -> "https://coreapi.uat.heb.com"
        Local -> "http://localhost:8081"
        FunctionalTest -> "http://localhost:8088"

    baseRoute e =
      case e of
        Prod -> Just "spur"
        PreProd -> Just "spur-shadow"
        UAT -> Just "spur-uat"
        Cert -> Just "spur-cert"
        Dev -> Just "spur-dev"
        Local -> Nothing
        FunctionalTest -> Nothing

main :: IO ()
main = do
  opts <- parseOptions
  let curlArgs =
        [ "-X"
        , verbToText (verb opts)
        , constructURL opts
        , "-H"
        , "Accept: " <> accept opts
        , "-H"
        , "Spur-Authorization: token " <> spurToken opts
        , "-H"
        , "apikey: " <> apiKey opts
        , "-H", "application: swagger-ui"
        ]
          <>
          -- Assume we're posting JSON for now
          case contentType opts of
            Just c -> ["-H", "Content-Type: " <> c]
            Nothing -> []
          <> case dataBody opts of
            Just d -> ["--data", d]
            Nothing -> []
          <> ["-i" | verbose opts]
          -- <> case application opts of
          --      Just a -> ["-H", "Application: " <> a]
          --      Nothing -> []

  void $ proc "curl" curlArgs empty
