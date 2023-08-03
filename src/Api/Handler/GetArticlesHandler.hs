{-# LANGUAGE OverloadedStrings #-}

module Api.Handler.GetArticlesHandler (getArticlesHandler)
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Text as Text
import Database.Redis (connect, defaultConnectInfo)
import Servant (err400, err404, throwError)
import Servant.Server (Handler, errBody)

import Api.Services.Gnews (requestToGnews)
import Api.Services.Redis (cacheData, fetchData)
import Api.Types (Response)
import Data.Char (isAlphaNum, isSpace)
import Data.Text.Encoding (encodeUtf8)

-- Returns the response from redis(cached data) if present for given parameters or fetch articles from Gnews
getArticlesHandler :: Maybe Text.Text -> Maybe Int -> Maybe Text.Text -> Handler Response
getArticlesHandler keyword articlesCount searchBasedOn = do
    connectionToRedis <- liftIO $ connect defaultConnectInfo
    validatedKeyword <- validateKeywordParameter keyword
    validatedCount <- validateArticlesCountParameter articlesCount
    validatedAttributes <- validateAttributeParameter searchBasedOn
    let keyForRedis = encodeUtf8 $ validatedKeyword <> Text.pack (show validatedCount) <> validatedAttributes
    dataFromRedis <- liftIO $ fetchData connectionToRedis keyForRedis
    case dataFromRedis of
        Nothing -> do
            response <- responseFromGnews validatedCount validatedKeyword validatedAttributes
            liftIO $ cacheData connectionToRedis keyForRedis response
            return response
        Just response -> return response

-- Returns the response from Gnews service for the given count of articles, keyword and attributes.
responseFromGnews :: Int -> Text.Text -> Text.Text -> Handler Response
responseFromGnews count keyword searchBasedOn = do
    resonseFromGnews <- liftIO $ requestToGnews count keyword searchBasedOn
    case resonseFromGnews of
        Left _ -> throwError $ err404{errBody = "Failed to get response from Gnews"}
        Right result -> return result

{- This parameter allows to specify your search keywords to find the news articles you are looking for.
   The keywords will be used to return the most relevant articles.
   It is possible to use logical operators with keywords -}
validateKeywordParameter :: Maybe Text.Text -> Handler Text.Text
validateKeywordParameter maybeKeyword =
    case maybeKeyword of
        Nothing -> throwError $ err400{errBody = "Keyword is a required query parameter"}
        Just keyword ->
            -- Check that all characters in keyword are of alphanumeric or space in between words.
            -- Stripping the keyword to remove leading and trailing white spaces.
            if Text.all (\char -> isAlphaNum char || isSpace char) (Text.strip keyword)
                then return $ Text.strip keyword
                else -- It is not possible to use special characters without putting quotes around the whole keyword.
                    return $ "\"" <> Text.strip keyword <> "\""

{- This parameter allows to specify the number of news articles returned by the API.
   The minimum value of this parameter is 1 and the maximum value is 100.
   Default value is 10. -}
validateArticlesCountParameter :: Maybe Int -> Handler Int
validateArticlesCountParameter maybeCount =
    case maybeCount of
        Nothing -> return 10
        Just count ->
            if count > 0 && count <= 100
                then return count
                else throwError $ err400{errBody = "Count should be greater than zero and less than or equal to 100"}

{- This parameter allows to choose in which attributes the keywords are searched.
   The attributes that can be set are title, description and content.
   It is possible to combine several attributes by separating them with a comma. -}
validateAttributeParameter :: Maybe Text.Text -> Handler Text.Text
validateAttributeParameter maybeAttribute =
    case maybeAttribute of
        Nothing -> return ""
        Just attributes -> do
            let splittedAttributes = Text.split (== ',') attributes
                validAttributeOrNot = all (\a -> a `elem` ["title", "description", "content"]) splittedAttributes
            if validAttributeOrNot
                then return attributes
                else
                    throwError $
                        err400
                            { errBody = "Attribute should be title, description, content or combinations of these seperated by commas"
                            }
