{-# LANGUAGE OverloadedStrings #-}

import System.Process (callProcess)
import Turtle

data KPSEnv
  = Prod
  | PreProd
  | UAT
  | Cert

dbName :: KPSEnv -> String
dbName env =
  case env of
    Prod -> "pspur-db"
    PreProd -> "rspur-db"
    UAT -> "uspur-db"
    Cert -> "cspur-db"

dbPort :: KPSEnv -> Int
dbPort env =
  case env of
    Prod -> 5436
    PreProd -> 5435
    UAT -> 5434
    Cert -> 5433

textToEnv :: Text -> Either Text KPSEnv
textToEnv envTxt =
  case envTxt of
    "prod" -> Right Prod
    "preprod" -> Right PreProd
    "uat" -> Right UAT
    "cert" -> Right Cert
    _ ->
      Left $
        "Unknown environment: \""
          <> envTxt
          <> "\". Must be one of prod, preprod, uat or cert"

parseEnv :: Text -> IO KPSEnv
parseEnv e =
  case textToEnv e of
    Right env -> pure env
    Left err -> die err

optParser :: Parser (Text, Bool)
optParser =
  (,)
    <$> argText "env" "The KPS environment (prod, preprod, uat or cert)"
    <*> switch "pgcli" 'p' "Use pgcli instead of psql"

main :: IO ()
main = do
  (envTxt, usePgCli) <- options "A psql wrapper for SPUR dbs" optParser
  env <- parseEnv envTxt
  let psqlArgs =
        [ "-h"
        , "localhost"
        , "-p"
        , show (dbPort env)
        , "-U"
        , "spurdbuser"
        , dbName env
        ]
      program = if usePgCli then "pgcli" else "psql"
  sh . liftIO $ callProcess program psqlArgs