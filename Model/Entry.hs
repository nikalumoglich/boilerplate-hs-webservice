{-# LANGUAGE DeriveGeneric #-}

module Model.Entry where

import Data.Aeson
import GHC.Generics
import Data.Text (Text)

data Entry = Entry { entryId :: Maybe Text, value :: Text } deriving (Show, Generic)

instance ToJSON Entry
instance FromJSON Entry