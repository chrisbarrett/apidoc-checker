module Apidoc.ParserSpec where

import qualified Apidoc.Parser        as Parser
import           Data.Validation
import           Paths_apidoc_checker as Paths
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
    describe "parsing an apidoc spec" $ do
        file <- runIO $ Paths.getDataFileName "resources/api.json"
        result <- runIO $ Parser.parseFile file
        it "parses successfully" $
          result `shouldSatisfy` isSuccess

isSuccess :: Validation e a -> Bool
isSuccess (Success _) = True
isSuccess (Failure _) = False
