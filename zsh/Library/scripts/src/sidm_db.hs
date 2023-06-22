{-# LANGUAGE OverloadedStrings #-}

import System.Process (callProcess)
import Turtle
import qualified Data.Text as T

data KPSEnv
  = Prod
  | PreProd
  | UAT
  | Cert
  | LocalDev
  | Dev
  deriving Eq

dbName :: KPSEnv -> String
dbName env =
  case env of
    Prod -> "pspur-rsf-db"
    PreProd -> "rspur-rsf-db"
    UAT -> "uspur-rsf-db"
    Cert -> "cspur-rsf-db"
    LocalDev -> "rsfdb"
    Dev -> "dspur-rsf-db"

dbPort :: KPSEnv -> Int
dbPort env =
  case env of
    Prod -> 5436
    PreProd -> 5435
    UAT -> 5434
    Cert -> 5433
    LocalDev -> 55329
    Dev -> 5432

textToEnv :: Text -> Either Text KPSEnv
textToEnv envTxt =
  case envTxt of
    "prod" -> Right Prod
    "preprod" -> Right PreProd
    "uat" -> Right UAT
    "cert" -> Right Cert
    "localdev" -> Right LocalDev
    "dev" -> Right Dev
    _ ->
      Left $
        "Unknown environment: \""
          <> envTxt
          <> "\". Must be one of prod, preprod, uat, cert, dev or localdev"

parseEnv :: Text -> IO KPSEnv
parseEnv e =
  case textToEnv e of
    Right env -> pure env
    Left err -> die err

optParser :: Parser (Text, Bool, Maybe Text)
optParser =
  (,,)
    <$> argText "env" "The KPS environment (prod, preprod, uat, cert, dev or localdev)"
    <*> switch "pgcli" 'p' "Use pgcli instead of psql"
    <*> optional (optText "command" 'c' "Run a SQL command")

main :: IO ()
main = do
  (envTxt, usePgCli, mbCommand) <- options "A psql wrapper for SIDM forwarder dbs" optParser
  env <- parseEnv envTxt
  let psqlArgs =
        [ "-h"
        , "localhost"
        , "-p"
        , show (dbPort env)
        , "-U"
        , if env == LocalDev then "rsfdbuser" else "sqlproxy"
        ] <> case mbCommand of
               Nothing -> []
               Just command -> ["-c", T.unpack command]
          <> [dbName env]
      program = if usePgCli then "pgcli" else "psql"
  sh . liftIO $ callProcess program psqlArgs
