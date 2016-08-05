module Apidoc.ParserSpec where

import qualified Apidoc.Check         as Check
import qualified Apidoc.Json          as Json
import           Control.Lens
import           Control.Lens.Extras
import           Paths_apidoc_checker as Paths
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
    describe "parsing an apidoc spec" $ do
        file <- runIO $ Paths.getDataFileName "resources/api.json"
        js <- runIO $ Json.parseFile file

        it "parses JSON successfully" $
            js `shouldSatisfy` is _Right

        let res = (Check.validate mempty <$> js) ^.. _Right
        it "has no errors" $
            res ^.. _head._1 `shouldSatisfy` is _Empty
        it "returns a spec" $
            res ^.. _head._2 `shouldSatisfy` isn't _Empty
