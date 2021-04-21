{-# LANGUAGE RecordWildCards #-}
module Hotwire.Turbo.Streams where

import Relude
import qualified Network.WebSockets as WS
import Data.Text
import Text.Blaze.Html
import Text.Blaze.Internal hiding (Append)

streamTag :: Html -> Html
streamTag = Parent "turbo-stream" "<turbo-stream" "</turbo-stream>"

targetAttr :: AttributeValue -> Attribute
targetAttr = customAttribute "target"

actionAttr :: AttributeValue -> Attribute
actionAttr = customAttribute "target"

data StreamAction
  = Append
  | Prepend
  | Replace
  | Update
  | Remove
  deriving (Show)

serializeStreamAction :: StreamAction -> AttributeValue
serializeStreamAction = textValue . toLower . show

data TurboStream
  = TurboStream
    { action :: StreamAction
    , target :: Text
    , innerHtml :: Text
    }

mkStream :: TurboStream -> Html
mkStream TurboStream{..} =
  streamTag ! targetAttr (textValue target) ! actionAttr (serializeStreamAction action) $
    text innerHtml
