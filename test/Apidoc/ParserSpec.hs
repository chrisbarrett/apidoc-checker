module Apidoc.ParserSpec where

import qualified Apidoc.Parser        as Parser
import qualified Data.ByteString      as BS
import qualified Data.Either          as Either
import           Paths_apidoc_checker as Paths
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
    describe "parsing a correctly formed apidoc spec" $ do
        file <- runIO $ Paths.getDataFileName "test/resources/1.json"
        json <- runIO $ BS.readFile file
        let result = Parser.parse json
        it "parses successfully" $
          pending
          -- result `shouldSatisfy` Either.isRight
