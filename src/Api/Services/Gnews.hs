{-#LANGUAGE OverloadedStrings #-}
module Api.Services.Gnews (requestToGnews) where

import Network.HTTP.Req

import Api.Types

-- apikey for Gnews service
apiKey :: String
apiKey = "05091a60f30ed1d8996c64d1eef3ff40"

-- request to Gnews service which return news articles
requestToGnews :: Int -> String -> String -> IO (Either String Response)
requestToGnews articlesCount searchStr title =
    runReq defaultHttpConfig  $  do
        r <- req GET
                    (https "gnews.io"/:"api"/:"v4"/:"search")
                    NoReqBody
                    jsonResponse
                    ("apikey" =: apiKey <>"q" =: searchStr <> "max" =: articlesCount <> "in" =: title)
        if responseStatusCode r == 200
            then return $ Right (responseBody r :: Response)
            else return $ Left "Failed to connect to gnews"
