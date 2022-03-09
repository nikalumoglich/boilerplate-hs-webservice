{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Model.Address where

import Data.Aeson
import GHC.Generics ( Generic )
import Data.Text (Text)
import qualified Data.ByteString.Lazy as LB

data Address = Address { street :: Maybe Text, cep :: Text, district :: Text, city :: Text, state :: Text } deriving (Show, Generic)

maybeAddress :: FromJSON a => LB.ByteString -> Maybe Address
maybeAddress = decode

instance ToJSON Address
instance FromJSON Address