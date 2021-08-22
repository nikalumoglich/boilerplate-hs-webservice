{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

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

import Data.Aeson (FromJSON, ToJSON)

import Database.MySQL.Base
import qualified System.IO.Streams as Streams
import qualified Data.Text as BSC
import Data.Maybe (fromJust)

data Entry = Entry { entryId :: Maybe String, value :: String } deriving (Show, Generic)

instance ToJSON Entry
instance FromJSON Entry

createEntity :: [MySQLValue] -> Maybe Entry
createEntity (_ : MySQLText entryId : MySQLText value : _) = Just Entry {
  entryId = Just $ BSC.unpack entryId
, value = BSC.unpack value
}
createEntity _ = Nothing


getEntryFromList :: [[MySQLValue]] -> Maybe Entry
getEntryFromList lst | null lst  = Nothing
                     | otherwise = createEntity $ head lst

findByEntryId :: MySQLConn -> BSC.Text -> IO (Maybe Entry)
findByEntryId conn entryId = do
  s <- prepareStmt conn "SELECT * FROM entries WHERE entryId = ?"
  (defs, is) <- queryStmt conn s [MySQLText entryId]
  let queryReturn = Streams.toList is
  getEntryFromList <$> queryReturn

listEntries :: MySQLConn -> IO [Maybe Entry]
listEntries conn = do
  s <- prepareStmt conn "SELECT * FROM entries"
  (defs, is) <- queryStmt conn s []
  let queryReturn = Streams.toList is
  map createEntity <$> queryReturn

saveEntry :: MySQLConn -> Entry -> IO OK
saveEntry conn entry = do
  s <- prepareStmt conn "INSERT INTO `hs_entry`.`entries` (`id`, `entryId`, `value`) VALUES(NULL, ?, ?)"
  executeStmt conn s [MySQLText $ BSC.pack $ fromJust $ entryId entry, MySQLText $ BSC.pack $ value entry]

updateEntry :: MySQLConn -> BSC.Text -> Entry -> IO OK
updateEntry conn entryId entry = do
  liftIO (putStrLn $ mconcat ["UPDATE `hs_entry`.`entries` SET `value` = '", value entry, "' WHERE entryId = '", BSC.unpack entryId, "'"])
  s <- prepareStmt conn "UPDATE `hs_entry`.`entries` SET `value` = ? WHERE entryId = ?"
  executeStmt conn s [MySQLText $ BSC.pack $ value entry, MySQLText entryId]

transactional :: MySQLConn -> IO a -> IO a
transactional conn procedure = mask $ \restore -> do
  execute_ conn "BEGIN"
  a <- restore procedure `onException` execute_ conn "ROLLBACK"
  execute_ conn "COMMIT"
  pure a

main :: IO ()
main = do

  conn <- connect
        defaultConnectInfo {ciUser = "haskelluser", ciPassword = "hspassword1", ciDatabase = "hs_entry"}

  scotty 3000 $ do

    post "/entry" $ do
      entry <- jsonData
      liftIO(findByEntryId conn (BSC.pack $ fromJust $ entryId entry)) >>= maybe
        (do
          liftIO(transactional conn $ saveEntry conn entry)
          status status201 >> json entry)
        (\_ -> do status status400 >> text "Entry Aleady Exist")

    put "/entry/:id" $ do
      id <- param "id"
      inputData <- jsonData
      liftIO(findByEntryId conn id) >>= maybe
        (do status status404 >> text "Entry not found")
        (\_ -> do
          liftIO(transactional conn $ updateEntry conn id inputData)
          -- let updatedEntry = Entry { entryId = Just $ BSC.unpack id, value = value inputData }
          status status204 >> text "deu certo?")-- json updatedEntry)

    get "/entry/:id" $ do
      id <- param "id"
      liftIO(findByEntryId conn id) >>= maybe
        (status status404 >> text "Entry not found")
        json

    get "/entry" $ do
      liftIO(listEntries conn) >>=
        json