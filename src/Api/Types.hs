{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeOperators #-}

module Api.Types (API, Response (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Servant.API (Get, JSON, QueryParam, (:>))

-- | API Structure
type API = GetArticles

type GetArticles =
    "articles"
        :> QueryParam "keyword" Text
        :> QueryParam "articlesCount" Int
        :> QueryParam "attribute" Text
        :> Get '[JSON] Response

-- Data type for the response from Gnews
data Response = Response
    { totalArticles :: Int
    , articles :: [Article]
    }
    deriving (Generic)

instance FromJSON Response
instance ToJSON Response

data Article = Article
    { title :: Text
    , description :: Text
    , content :: Text
    , url :: Text
    , image :: Text
    , publishedAt :: UTCTime
    , source :: Source
    }
    deriving (Generic)

instance FromJSON Article
instance ToJSON Article

data Source = Source
    { name :: Text
    , url :: Text
    }
    deriving (Generic)

instance FromJSON Source
instance ToJSON Source
