{-# LANGUAGE OverloadedStrings #-}

module Main where

import Model.Entry
import Model.Address
import Repository.EntryRepository

import GHC.Generics
import Control.Monad.IO.Class
import Control.Exception
import Control.Monad         (forever)

import Web.Scotty
import Network.HTTP.Types.Status

import qualified Data.Text.Lazy as T
import qualified Data.ByteString.Lazy.Char8 as C

import Data.Monoid (mconcat)
import Data.List ( find )

import Text.Read (readMaybe)

import Data.Aeson (FromJSON, ToJSON, decode)


import qualified Data.Text as BSC
import Data.Maybe (fromJust)

import Database.MySQL.Base

import Network.HTTP.Simple
import qualified Data.Text.Lazy.Encoding as TE
import qualified Data.ByteString.Char8 as IBS

transactional :: MySQLConn -> IO a -> IO a
transactional conn procedure = mask $ \restore -> do
  execute_ conn "BEGIN"
  a <- restore procedure `onException` execute_ conn "ROLLBACK"
  execute_ conn "COMMIT"
  pure a

endpoint :: [Char]
endpoint = "am.apidev.ewally.com.br"

makeRequest :: MonadIO m => IBS.ByteString -> m (Response C.ByteString)
makeRequest path = httpLBS (setRequestHost "am.apidev.ewally.com.br" $ setRequestPath path defaultRequest)

main :: IO ()
main = do

  conn <- connect
        defaultConnectInfo {ciUser = "haskelluser", ciPassword = "hspassword1", ciDatabase = "hs_entry"}

  scotty 3000 $ do

    post "/entry" $ do
      entry <- jsonData
      liftIO(findByEntryId conn (fromJust $ entryId entry)) >>= maybe
        (do
          liftIO(transactional conn $ saveEntry conn entry)
          status status201 >> json entry)
        (\_ -> status status400 >> text "Entry Already Exist")

    put "/entry/:id" $ do
      id <- param "id"
      inputData <- jsonData
      liftIO(findByEntryId conn id) >>= maybe
        (status status404 >> text "Entry not found")
        (\_ -> do
          liftIO(transactional conn $ updateEntry conn id inputData)
          status status204 >> text "deu certo?")

    get "/entry/:id" $ do
      id <- param "id"
      liftIO(findByEntryId conn id) >>= maybe
        (status status404 >> text "Entry not found")
        json

    get "/entry" $
      liftIO(listEntries conn) >>=
      json

    get "/httprequest/:cep" $ do
      _cep <- param "cep"
      let requestURL = IBS.pack $ "cep/" ++ _cep
      response <- makeRequest requestURL
      let status = getResponseStatusCode response
      if status == 200
        then do
          let jsonBody = getResponseBody response
          json $ TE.decodeUtf8 jsonBody
      else text "Entry Already Exist"