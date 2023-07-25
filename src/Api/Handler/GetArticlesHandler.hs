{-# LANGUAGE OverloadedStrings #-}
module Api.Handler.GetArticlesHandler
  (getArticlesHandler)
where

import Servant.Server (ServerError, Handler, errHeaders, errBody)
import Servant (throwError, err400)
import Control.Monad.IO.Class
import Data.Aeson (encode, (.=), object, Value (Null))

import Api.Types
import Api.Services.Gnews

-- return the response for given keyword, count and search attribute
getArticlesHandler :: String -> Maybe Int -> Maybe String -> Handler Response
getArticlesHandler keyword articlesCount searchBasedOn = do
    count <- validateArticlesCountParameter articlesCount
    case searchBasedOn of
        Nothing -> responseFromGnews count keyword ""
        Just attribute -> responseFromGnews count keyword attribute
    
responseFromGnews :: Int -> String -> String -> Handler Response
responseFromGnews count keyword searchBasedOn = do
    resonseFromGnews <- liftIO $ requestToGnews count keyword searchBasedOn
    case resonseFromGnews of
        Left _ -> throwError $ jsonError400 "Failed to get response from Gnews"
        Right result -> return result

validateArticlesCountParameter :: Maybe Int -> Handler Int
validateArticlesCountParameter maybeCount = 
    case maybeCount of
        Nothing -> return 10 --default value if this query parameter is absent
        Just count -> 
            if count > 0 && count <= 100
                then return count
            else throwError $ jsonError400 "Invalid count"

-- |400 : Bad Request.The server can't return a response due to an error on the client's end
jsonError400 :: String -> ServerError
jsonError400  msg = err400 { errBody=encode (body 400 msg)
                           , errHeaders = [("Content-Type", "application/json")]
                           }
    where 
        body code message = object ["code".=(code::Int),"error".= message,"result".= Null]
