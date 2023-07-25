{-#LANGUAGE DuplicateRecordFields #-}
{-#LANGUAGE DataKinds #-} -- for "articles"
{-#LANGUAGE TypeOperators #-} -- for :>
{-#LANGUAGE DeriveGeneric #-}
module Api.Types (API, Response(..)) where

import Servant.API (Get, (:>), JSON, QueryParam, Capture)
import Data.Time ( UTCTime )
import GHC.Generics ( Generic )
import Data.Aeson ( FromJSON, ToJSON )

-- | API Structure
type API = GetArticles

type GetArticles =
    "articles"
    :> Capture "keyword" String 
    :> QueryParam "articlesCount" Int
    :> QueryParam "attribute" String
    :> Get '[JSON] Response 

-- Data type for the response from Gnews
data Response = Response 
  { totalArticles :: Int
  , articles :: [Article]
  } deriving (Generic)

instance FromJSON Response
instance ToJSON Response

data Article = Article
  { title :: String
  , description :: String
  , content :: String
  , url :: String
  , image :: String
  , publishedAt :: UTCTime
  , source :: Source
  } deriving (Generic)

instance FromJSON Article
instance ToJSON Article

data Source = Source
  { name :: String
  , url :: String
  } deriving (Generic)

instance FromJSON Source
instance ToJSON Source
