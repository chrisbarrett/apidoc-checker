module Apidoc.ParserSpec where

import qualified Apidoc.Check         as Check
import qualified Apidoc.Json          as Json
import           Apidoc.Json.Lenses
import           Control.Lens
import           Control.Lens.Extras
import qualified Data.Sequence        as Seq
import           Paths_apidoc_checker as Paths
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    let adaptErrs = set _Left Seq.empty

    describe "parsing an apidoc spec" $ do
        file <- runIO $ Paths.getDataFileName "resources/api.json"
        js <- runIO $ Json.parseFile file

        it "parses JSON successfully" $
            js `shouldSatisfy` is _Right

        let res = Check.validate =<< (adaptErrs js)
        it "returns a spec" $
            res `shouldSatisfy` is _Right

    describe "parsing a malformed apidoc spec" $ do
        file <- runIO $ Paths.getDataFileName "resources/malformed.json"
        js <- runIO $ Json.parseFile file

        it "parses JSON successfully" $
            js `shouldSatisfy` is _Right

        let js' = js ^.. _Right._JArray._2._head
            res = Check.validate (head js')
        it "is left" $
            res `shouldSatisfy` is _Left
        it "returns a non-empty list of errors" $
            res ^.. _Left `shouldSatisfy` isn't _Empty
        it "returns many errors" $
            head (res ^.. _Left.to length) `shouldSatisfy` (> 1)
