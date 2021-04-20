module Main where

import Relude
import Data.Maybe
import Control.Concurrent
import Colourista.Pure
import Colourista.IO
import System.Random
import qualified Web.Scotty as Sc
import qualified Data.Text.Lazy as LT
-- import Network.HTTP.Types
import qualified Network.Wai.Middleware.Gzip as Sc
import qualified Network.Wai.Handler.WebSockets as WaiWs
import qualified Network.WebSockets as WS
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import Hotwire.Turbo (trimFrame)

main :: IO ()
main = do
  let port = 80
  let settings = Warp.setPort port Warp.defaultSettings
  sapp <- scottyApp
  putStrLn . formatWith [bold, green] $ "Running scotty app on port " <> show port <> " ✨✨✨"
  Warp.runSettings settings $ WaiWs.websocketsOr WS.defaultConnectionOptions wsapp sapp

logRequest :: MonadIO m => Text -> m ()
logRequest t = liftIO $ blueMessage $ "↳ " <> t

scottyApp :: IO Wai.Application
scottyApp =
  Sc.scottyApp $ do
    Sc.middleware $ Sc.gzip $ Sc.def { Sc.gzipFiles = Sc.GzipCompress }


    Sc.get "/" $ do
      logRequest "GET - /"
      Sc.file "demo/index.html"
    Sc.get "/index.js" $ do
      logRequest "GET - /index.js"
      Sc.file "demo/hotwire-client/index.js"
    Sc.get "/next" $ do
      logRequest "GET - /next"
      Sc.file "demo/next.html"
    Sc.get "/frame/word" $ do
      logRequest "GET - /frame/word"
      frameHeader <- Sc.header "Turbo-Frame"
      liftIO $ case frameHeader of
        Nothing -> warningMessage "expected Turbo-Frame header, but didn't find it"
        Just header -> successMessage $ "found Turbo-Frame header: " <> show header
      num <- liftIO randomIO
      let word = fromJust $ (["monkey", "banana", "simbad", "church", "towel"] :: [LT.Text]) !!? (num `mod` 4)
      Sc.html $ maybe id trimFrame frameHeader
              $ fold [ "<div><div><div>"
                     , "<turbo-frame id=\"irrelevant\">"
                     , "<turbo-frame id=\"word_frame\">"
                     , "<h1>This is a frame</h1>"
                     , "<p>" <> word <> "</p>"
                     , "<a href=\"/frame/word\">Get a new word</a>"
                     , "</turbo-frame>"
                     , "</turbo-frame>"
                     , "</div></div></div>"
                     ]

wsapp :: WS.ServerApp
wsapp pending = do
  putText "ws connected"
  conn <- WS.acceptRequest pending
  WS.withPingThread conn 30 (pure ()) (pure ())

  (msg :: Text) <- WS.receiveData conn
  WS.sendTextData conn $ ("initial> " :: Text) <> msg

  forever $ do
    WS.sendTextData conn ("loop data" :: Text)
    threadDelay $ 1 * 1000000
