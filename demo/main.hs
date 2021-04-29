module Main where

import Colourista.IO
import Colourista.Pure
import Control.Concurrent
import Data.Maybe
import qualified Data.Text.Lazy as LT
-- import Network.HTTP.Types

import Hotwire.Turbo.Frames (trimFrame)
import Hotwire.Turbo.Streams
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WaiWs
import qualified Network.Wai.Middleware.Gzip as MW
import qualified Network.WebSockets as WS
import Relude
import System.Random
import qualified Web.Scotty as Sc

main :: IO ()
main = do
  let port = 8081
  let settings = Warp.setPort port Warp.defaultSettings
  sapp <- scottyApp
  putStrLn . formatWith [bold, green] $ "λ🔌 Running haskwire demo on port " <> show port <> " ✨✨✨"
  Warp.runSettings settings $ WaiWs.websocketsOr WS.defaultConnectionOptions wsapp sapp

logRequest :: MonadIO m => Text -> m ()
logRequest t = liftIO $ blueMessage $ "↳ " <> t

getRandomWord :: MonadIO m => m LT.Text
getRandomWord = do
  num <- liftIO randomIO
  pure . fromJust $ (["monkey", "banana", "sinbad", "church", "towel"] :: [LT.Text]) !!? (num `mod` 5)

scottyApp :: IO Wai.Application
scottyApp =
  Sc.scottyApp $ do
    Sc.middleware $ MW.gzip $ MW.def {MW.gzipFiles = MW.GzipCompress}

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
      word <- getRandomWord
      Sc.html $
        maybe id trimFrame frameHeader $
          fold
            [ "<div><div><div>",
              "<turbo-frame id=\"irrelevant\">",
              "<turbo-frame id=\"word_frame\">",
              "<h1>This is a frame</h1>",
              "<p>" <> word <> "</p>",
              "<a href=\"/frame/word\">Get a new word</a>",
              "</turbo-frame>",
              "</turbo-frame>",
              "</div></div></div>"
            ]

wsapp :: WS.ServerApp
wsapp pending = do
  sendTurboMsg <- turboMsg <$> turboConn pending
  forever $ do
    word <- getRandomWord
    sendTurboMsg $
      TurboStream
        { target = "stream-test",
          action = Update $ LT.toStrict word
        }
    threadDelay $ 1 * 1000000
