module Server (runServer) where

import Data.Proxy (Proxy (..))
import Network.Wai.Handler.Warp (run)
import Servant.Server (Server, serve)

import Api.Handler.GetArticlesHandler (getArticlesHandler)
import Api.Types (API)

runServer :: IO ()
runServer = run 8001 (serve api server)

api :: Proxy API
api = Proxy

server :: Server API
server = getArticlesHandler
