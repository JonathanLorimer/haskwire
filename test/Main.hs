module Main (main) where

import Test.Hspec.Runner
import qualified Spec
import Relude

main :: IO ()
main = hspecWith defaultConfig Spec.spec
