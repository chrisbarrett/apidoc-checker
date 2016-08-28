{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
module Apidoc.JsonSpec where

import           Apidoc.Json
import qualified Apidoc.Pos           as Pos
import           Control.Lens
import           Control.Lens.Extras  (is)
import qualified Data.ByteString      as BS
import qualified Data.Map             as Map
import           Paths_apidoc_checker as Paths
import           Test.Hspec

main :: IO ()
main = hspec spec


spec :: Spec
spec = do

    let itParsesSuccessfully result =
          it "parses successfully" $
              result `shouldSatisfy` is _Right

        itFailsToParse result =
              it "fails to runParse" $
                  result `shouldSatisfy` is _Left

    describe "parsing null" $ do
        let result = parseEither "null"
        itParsesSuccessfully result
        it "parses to null" $
            result `shouldSatisfy` (\ (Right JNull {}) -> True)

    describe "parsing booleans" $ do

        describe "true" $ do
            let result = parseEither "true"
            itParsesSuccessfully result
            it "parses to true" $
                result `shouldSatisfy` (\ (Right (JBool _ b)) -> b)

        describe "false" $ do
            let result = parseEither "false"
            itParsesSuccessfully result
            it "parses to false" $
                result `shouldSatisfy` (\ (Right (JBool _ b)) -> not b)

    describe "parsing numbers" $ do

        let itParsesToNumber expected result = do
              itParsesSuccessfully result

              it "parses to a number" $
                  result `shouldSatisfy` isNumber

              it "parses to the expected number" $
                  result `shouldParseToNumber` expected

            isNumber (Right JNumber {}) = True
            isNumber _ = False

            shouldParseToNumber (Right (JNumber _ result)) expected = result `shouldBe` expected
            shouldParseToNumber res _ = error (show res)

        describe "integers" $ do

            context "zero" $ do
                let result = parseEither "0"
                itParsesToNumber 0 result

            context "unsigned" $ do
                let result = parseEither "12345"
                itParsesToNumber 12345 result

            context "unary minus" $ do
                let result = parseEither "-12345"
                itParsesToNumber (-12345) result

            context "positive exponent" $ do
                let result = parseEither "12345e+1"
                itParsesToNumber 12345e+1 result

            context "negative exponent" $ do
                let result = parseEither "12345e-1"
                itParsesToNumber 12345e-1 result

            context "leading zero" $ do
                let result = parseEither "0123"
                itFailsToParse result

            context "unary plus" $ do
                let result = parseEither "+12345"
                itFailsToParse result


        describe "decimals" $ do

            context "zero" $ do
                let result = parseEither "0.0"
                itParsesToNumber 0.0 result

            context "unsigned" $ do
                let result = parseEither "123.45"
                itParsesToNumber 123.45 result

            context "unary minus" $ do
                let result = parseEither "-123.45"
                itParsesToNumber (-123.45) result

            context "positive exponent" $ do
                let result = parseEither "123.45e+1"
                itParsesToNumber 123.45e+1 result

            context "negative exponent" $ do
                let result = parseEither "123.45e-1"
                itParsesToNumber 123.45e-1 result

            context "leading zero" $ do
                let result = parseEither "01.23"
                itFailsToParse result

    describe "parsing strings" $ do

        let itParsesToString expected result = do
              itParsesSuccessfully result

              it "parses to a string" $
                  result `shouldSatisfy` isString

              it "parses to the expected string" $
                  result `shouldParseToString` expected

            isString (Right JString {}) = True
            isString _ = False

            shouldParseToString (Right (JString _ result)) expected = result `shouldBe` expected
            shouldParseToString res _ = error (show res)

        context "alphabetic string" $ do
            let result = parseEither "\"hello\""
            itParsesToString "hello" result

        context "alphanumeric string" $ do
            let result = parseEither "\"hello 123\""
            itParsesToString "hello 123" result

        context "escape sequences" $ do
            let result = parseEither "\"hello\nworld\""
            itParsesToString "hello\nworld" result

    describe "parsing arrays" $ do

        let itParsesToArray expected result = do
              itParsesSuccessfully result

              it "parses to an array" $
                  result `shouldSatisfy` isArray

              it "parses to the expected array" $
                  result `shouldParseToArray` expected

            isArray (Right JArray {}) = True
            isArray _ = False

            shouldParseToArray (Right (JArray _ result)) expected = result `shouldBe` expected
            shouldParseToArray res _ = error (show res)

        context "empty array" $ do
            let result = parseEither "[]"
            itParsesToArray [] result

        context "empty array with internal padding" $ do
            let result = parseEither "[  ]"
            itParsesToArray [] result

        context "singleton array" $ do
            let result = parseEither "[1]"
            itParsesToArray [JNumber Pos.empty 1] result

        context "heterogenous array" $ do
            let result = parseEither "[true, null, false]"
            itParsesToArray [JBool Pos.empty True , JNull Pos.empty, JBool Pos.empty False] result

    describe "parsing objects" $ do

        let itParsesToObject expected result = do
              itParsesSuccessfully result

              it "parses to an object" $
                  result `shouldSatisfy` isObject

              it "parses to the expected object" $
                  result `shouldParseToObject` expected

            isObject (Right JObject {}) = True
            isObject _ = False

            shouldParseToObject (Right (JObject _ result)) expected = result `shouldBe` expected
            shouldParseToObject res _ = error (show res)

        context "empty object" $ do
            let result = parseEither "{}"
            itParsesToObject (Object Pos.empty mempty) result

        context "empty object with internal padding" $ do
            let result = parseEither "{  }"
            itParsesToObject (Object Pos.empty mempty) result

        context "singleton object" $ do
            let result = parseEither "{\"foo\":\"bar\"}"
                expected = Object Pos.empty [(Key Pos.empty "foo", JString Pos.empty "bar")]
            itParsesToObject expected result

        context "heterogenous object" $ do
            let result = parseEither "{\"foo\":\"bar\", \"baz\": null}"
                expected = Object Pos.empty [ (Key Pos.empty "foo", JString Pos.empty "bar")
                                            , (Key Pos.empty "baz", JNull Pos.empty)
                                            ]
            itParsesToObject expected result

    describe "parsing a twitter timeline response" $ do
        let path = "resources/twitter-timeline.json"
        file <- runIO (Paths.getDataFileName path)
        json <- runIO (BS.readFile file)
        let result = parseEither json
        itParsesSuccessfully result
