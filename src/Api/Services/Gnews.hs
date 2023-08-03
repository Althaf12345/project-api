{-# LANGUAGE OverloadedStrings #-}

module Api.Services.Gnews (requestToGnews) where

import Data.Text (Text)
import Network.HTTP.Req (
    GET (GET),
    NoReqBody (NoReqBody),
    defaultHttpConfig,
    https,
    jsonResponse,
    req,
    responseBody,
    responseStatusCode,
    runReq,
    (/:),
    (=:),
 )

import Api.Types (Response)

-- apikey for Gnews service
apiKey :: String
apiKey = "05091a60f30ed1d8996c64d1eef3ff40"

-- request to Gnews service which return news articles
requestToGnews :: Int -> Text -> Text -> IO (Either Text Response)
requestToGnews articlesCount searchStr attribute =
    runReq defaultHttpConfig $ do
        r <-
            req
                GET
                (https "gnews.io" /: "api" /: "v4" /: "search")
                NoReqBody
                jsonResponse
                ("apikey" =: apiKey <> "q" =: searchStr <> "max" =: articlesCount <> "in" =: attribute)
        if responseStatusCode r == 200
            then return $ Right (responseBody r :: Response)
            else return $ Left "Failed to connect to gnews"
