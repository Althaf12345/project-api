module Api.Main (startApp) where

import Data.Proxy ( Proxy(..) )
import Servant.Server (Application, Server, Context(EmptyContext), serveWithContext)
import Network.Wai.Handler.Warp (run, Port)

import Api.Types ( API )
import Api.Handler.GetArticlesHandler ( getArticlesHandler )

startApp :: IO ()
startApp = run portNumber application

-- Port where the application runs
portNumber :: Port
portNumber = 8001

application :: Application
application = serveWithContext
    api
    EmptyContext
    server

api :: Proxy API
api = Proxy

server :: Server API
server = getArticlesHandler
