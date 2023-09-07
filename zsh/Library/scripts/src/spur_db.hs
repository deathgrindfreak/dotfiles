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
  | FunctionalTest

dbName :: KPSEnv -> String
dbName env =
  case env of
    Prod -> "pspur-db"
    PreProd -> "rspur-db"
    UAT -> "uspur-db"
    Cert -> "cspur-db"
    LocalDev -> "spurdb"
    Dev -> "dspur-db-v13"
    FunctionalTest -> "spurdb"

dbPort :: KPSEnv -> Int
dbPort env =
  case env of
    Prod -> 5436
    PreProd -> 5435
    UAT -> 5434
    Cert -> 5433
    LocalDev -> 54329
    Dev -> 5432
    FunctionalTest -> 54320

textToEnv :: Text -> Either Text KPSEnv
textToEnv envTxt =
  case envTxt of
    "prod" -> Right Prod
    "preprod" -> Right PreProd
    "uat" -> Right UAT
    "cert" -> Right Cert
    "localdev" -> Right LocalDev
    "dev" -> Right Dev
    "functional_test" -> Right FunctionalTest
    _ ->
      Left $
        "Unknown environment: \""
          <> envTxt
          <> "\". Must be one of prod, preprod, uat, cert, dev, localdev or functional_test"

parseEnv :: Text -> IO KPSEnv
parseEnv e =
  case textToEnv e of
    Right env -> pure env
    Left err -> die err

optParser :: Parser (Text, Bool, [Text], [Text])
optParser =
  (,,,)
    <$> argText "env" "The KPS environment (prod, preprod, uat, cert, dev, localdev or functional_test)"
    <*> switch "pgcli" 'p' "Use pgcli instead of psql"
    <*> many (optText "command" 'c' "Run a SQL command")
    <*> many (optText "file" 'f' "Run a SQL file")

main :: IO ()
main = do
  (envTxt, usePgCli, commands, files) <- options "A psql wrapper for SPUR dbs" optParser
  env <- parseEnv envTxt
  let psqlArgs =
        [ "-h"
        , "localhost"
        , "-p"
        , show (dbPort env)
        , "-U"
        , "spurdbuser"
        ] <> concatMap (\command -> ["-c", T.unpack command]) commands
          <> concatMap (\file -> ["-f", T.unpack file]) files
          <> [dbName env]
      program = if usePgCli then "pgcli" else "psql"
  sh . liftIO $ callProcess program psqlArgs
