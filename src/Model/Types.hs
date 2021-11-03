{-# LANGUAGE DeriveGeneric #-}

module Model.Types
  ( Config(..)
  , SpecialArg(..)
  ) where

import Data.Aeson (FromJSON)
import Data.Map (Map)
import Data.Text (Text)
import GHC.Generics (Generic)

data SpecialArg =
  SpecialArg
    { route :: [Text]
    , refreshToken :: Text
    }
  deriving (Generic)

data Config =
  Config
    { userAgent :: Text
    , clientId :: Text
    , secret :: Text
    , path :: Text
    , specialArgs :: Map Text SpecialArg
    , aria2Secret :: Maybe Text
    }
  deriving (Generic)

instance FromJSON SpecialArg

instance FromJSON Config
