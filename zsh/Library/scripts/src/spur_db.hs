{-# LANGUAGE OverloadedStrings #-}

import Turtle
import System.Process (callProcess)

data KPSEnv = Prod
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
    _ -> Left $ "Unknown environment: \""
                  <> envTxt
                  <> "\". Must be one of prod, preprod, uat or cert"

parseEnv :: Text -> IO KPSEnv
parseEnv e =
  case textToEnv e of
    Right env -> pure env
    Left err -> die err

optParser :: Parser Text
optParser = argText "env" "The KPS environment (prod, preprod, uat or cert)"

main :: IO ()
main = do
  env <- parseEnv =<< options "A psql wrapper for SPUR dbs" optParser
  let psqlArgs =
        [ "-h", "localhost"
        , "-p", show (dbPort env)
        , "-U", "spurdbuser"
        , dbName env
        ]
  sh . liftIO $ callProcess "psql" psqlArgs
