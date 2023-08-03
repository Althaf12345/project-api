module Api.Services.Redis (cacheData, fetchData)
where

import Control.Monad (void)
import Data.Aeson (decodeStrict)
import Data.Aeson.Extra (encodeStrict)
import Data.ByteString.Char8 (ByteString)
import Database.Redis (Connection, get, runRedis, setex)

import Api.Types (Response)

-- Cache data into redis if the data is not present in redis
cacheData :: Connection -> ByteString -> Response -> IO ()
cacheData connection keyForRedis responseToCache =
    runRedis connection $ void $ setex keyForRedis 3600 (encodeStrict responseToCache)

-- Fetch cached data from redis if present for the given key
fetchData :: Connection -> ByteString -> IO (Maybe Response)
fetchData connection keyForRedis =
    runRedis connection $ do
        result <- get keyForRedis
        case result of
            Right (Just response) -> return (decodeStrict response :: Maybe Response)
            _ -> return Nothing
