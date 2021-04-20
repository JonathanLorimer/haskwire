module Hotwire.Turbo.FramesSpec where

import Relude hiding (Text)
import Data.Text.Lazy
import Test.Hspec
import Hotwire.Turbo.Frames

spec :: Spec
spec = do
  context "Turbo-Frame" $ do
    describe "trimFrame" $ do
      it "should return the same html when the outermost tag is a turbo-frame" $ do
        trimFrame mockTurboFrameId mockTurboFrame `shouldBe` mockTurboFrame
      it "should trim external tags such that the outermost tag is a turbo-frame" $ do
        let withSurrounding =
              fold [ "<div><div><div>"
                   , mockTurboFrame
                   , "</div></div></div>"
                   ]
        trimFrame mockTurboFrameId withSurrounding `shouldBe` mockTurboFrame
      it "should trim external turbo-frames such that the outermost tag has the correct id" $ do
        let withSurrounding =
              fold [ "<turbo-frame id=\"wrong-id\">"
                   , mockTurboFrame
                   ,  "</turbo-frame>"
                   ]
        trimFrame mockTurboFrameId withSurrounding `shouldBe` mockTurboFrame
      it "should not terminate on internal turbo-frames when trimming" $ do
        let withInternal = mkMockTurboFrame $ "<turbo-frame id=\"wrong-id\"></turbo-frame>"
        trimFrame mockTurboFrameId withInternal `shouldBe` withInternal


mockTurboFrameId :: Text
mockTurboFrameId = "mock-frame-id"

mockTurboFrame :: Text
mockTurboFrame = mkMockTurboFrame $
  fold [ "<h1>This is a frame</h1>"
       , "<p>this is some text </p>"
       , "<a href=\"/frame/word\">Get a new word</a>"
       ]

mkMockTurboFrame :: Text -> Text
mkMockTurboFrame internal =
  fold [ "<turbo-frame id=\"" <> mockTurboFrameId<> "\">"
       , internal
       , "</turbo-frame>"
       ]
