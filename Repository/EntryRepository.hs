{-# LANGUAGE OverloadedStrings #-}

module Repository.EntryRepository where

import Model.Entry

import Data.Maybe (fromJust)

import Data.Text (Text)
import Database.MySQL.Base
import qualified System.IO.Streams as Streams

createEntity :: [MySQLValue] -> Maybe Entry
createEntity (_ : MySQLText entryId : MySQLText value : _) = Just Entry {
  entryId = Just entryId
, value = value
}
createEntity _ = Nothing

findByEntryId :: MySQLConn -> Text -> IO (Maybe Entry)
findByEntryId conn entryId = do
  s <- prepareStmt conn "SELECT * FROM entries WHERE entryId = ?"
  (defs, is) <- queryStmt conn s [MySQLText entryId]
  let queryReturn = Streams.toList is
  getEntryFromList <$> queryReturn
  where
      getEntryFromList :: [[MySQLValue]] -> Maybe Entry
      getEntryFromList lst | null lst  = Nothing
                           | otherwise = createEntity $ head lst

listEntries :: MySQLConn -> IO [Maybe Entry]
listEntries conn = do
  s <- prepareStmt conn "SELECT * FROM entries"
  (defs, is) <- queryStmt conn s []
  let queryReturn = Streams.toList is
  map createEntity <$> queryReturn

saveEntry :: MySQLConn -> Entry -> IO OK
saveEntry conn entry = do
  s <- prepareStmt conn "INSERT INTO `hs_entry`.`entries` (`id`, `entryId`, `value`) VALUES(NULL, ?, ?)"
  executeStmt conn s [MySQLText $ fromJust $ entryId entry, MySQLText $ value entry]

updateEntry :: MySQLConn -> Text -> Entry -> IO OK
updateEntry conn entryId entry = do
  s <- prepareStmt conn "UPDATE `hs_entry`.`entries` SET `value` = ? WHERE entryId = ?"
  executeStmt conn s [MySQLText $ value entry, MySQLText entryId]