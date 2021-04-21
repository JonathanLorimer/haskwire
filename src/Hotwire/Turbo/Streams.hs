{-# LANGUAGE RecordWildCards #-}
module Hotwire.Turbo.Streams where

import Relude hiding (Text)
import qualified Network.WebSockets as WS
import Data.Text
import Text.Blaze.Html
import Text.Blaze.Html.Renderer.Text
import Text.Blaze.Internal hiding (Append)

turboStream :: Html -> Html
turboStream = Parent "turbo-stream" "<turbo-stream" "</turbo-stream>"

template :: Html -> Html
template = Parent "template" "<template" "</template>"

targetAttr :: AttributeValue -> Attribute
targetAttr = customAttribute "target"

actionAttr :: AttributeValue -> Attribute
actionAttr = customAttribute "action"

data StreamAction
  = Append Text
  | Prepend Text
  | Replace Text
  | Update Text
  | Remove
  deriving (Show, Eq)

serializeStreamAction :: StreamAction -> AttributeValue
serializeStreamAction action =
  case action of
    Append _ -> textValue "append"
    Prepend _ -> textValue "prepend"
    Replace _ -> textValue "replace"
    Update _ -> textValue "update"
    Remove -> textValue "remove"

getInnerHtml :: StreamAction -> Html
getInnerHtml action =
  case action of
    Append innerHtml -> template $ text innerHtml
    Prepend innerHtml -> template $ text innerHtml
    Replace innerHtml -> template $ text innerHtml
    Update innerHtml -> template $ text innerHtml
    Remove -> mempty

data TurboStream
  = TurboStream
    { action :: StreamAction
    , target :: Text
    }

mkStreamText :: TurboStream -> Text
mkStreamText TurboStream{..} =
  toStrict . renderHtml $
    turboStream ! targetAttr (textValue target) ! actionAttr (serializeStreamAction action) $
       getInnerHtml action

mkStreamHtml :: TurboStream -> Html
mkStreamHtml TurboStream{..} =
  turboStream ! targetAttr (textValue target) ! actionAttr (serializeStreamAction action) $
     getInnerHtml action


