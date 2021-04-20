{-# LANGUAGE RecordWildCards #-}
module Hotwire.Turbo where

import Relude hiding (Text, Nat)
import Data.Text.Lazy (Text)
import Control.Monad.Loops (takeWhileM)
import Data.Nat
import Text.HTML.Parser

-- $turboframe
-- Most of the turbo-frame logic is handled client side, but there are some convenience functions
-- we can provide that make life better. For example we can trim down the response in case of endpoint
-- re-use.

-- | A utility function for plucking out the @turbo-frame@ from a chunk of html.
-- This prevents the user from sending unnecessarily heavy responses over the wire.
trimFrame :: Text -> Text -> Text
trimFrame frameId = renderTokens . findFrame frameId . parseTokensLazy
  where
    findFrame :: Text -> [Token] -> [Token]
    findFrame fId = flip evalState Z
                  . takeWhileM (fmap not . isTurboFrameClosingTagNested)
                  . dropWhile (\tok -> not $ isTurboFrameTag tok && tokenHasSameId fId tok)

    isTurboFrameClosingTagNested :: Token -> State Nat Bool
    isTurboFrameClosingTagNested tok = do
      openTagCount <- get
      if | isTurboFrameTag tok -> modify S >> pure False
         | isTurboFrameClosingTag tok ->
           case openTagCount of
             Z -> pure True
             S nat -> put nat >> pure False
         | otherwise ->
           case openTagCount of
             Z -> pure True
             _ -> pure False

    isTurboFrameClosingTag :: Token -> Bool
    isTurboFrameClosingTag (TagClose tname) = tname == "turbo-frame"
    isTurboFrameClosingTag _ = False

    isTurboFrameTag :: Token -> Bool
    isTurboFrameTag (TagOpen tname _) = tname == "turbo-frame"
    isTurboFrameTag _ = False

    hasSameId :: Text -> Attr -> Bool
    hasSameId fId (Attr attrName attr) = attrName == "id" && attr == toStrict fId

    tokenHasSameId :: Text -> Token -> Bool
    tokenHasSameId fId (TagOpen _ attrs) = any (hasSameId fId) attrs
    tokenHasSameId fId (TagSelfClose _ attrs) = any (hasSameId fId) attrs
    tokenHasSameId _ _ = False

