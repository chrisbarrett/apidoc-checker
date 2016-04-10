{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
module Apidoc.JsonSpec where

import           Apidoc.Json
import qualified Data.ByteString      as BS
import qualified Data.Either          as Either
import qualified Data.Map             as Map
import           Paths_apidoc_checker as Paths
import           Test.Hspec

main :: IO ()
main = hspec spec


spec :: Spec
spec = do

    let itParsesSuccessfully result =
          it "parses successfully" $
              result `shouldSatisfy` Either.isRight

        itFailsToParse result =
              it "fails to runParse" $
                  result `shouldSatisfy` Either.isLeft

        runParse = (fmap.fmap) eraseSpans parse


    describe "parsing null" $ do
        let result = runParse "null"
        itParsesSuccessfully result
        it "parses to null" $
            result `shouldSatisfy` (\ (Right JNull {}) -> True)

    describe "parsing booleans" $ do

        describe "true" $ do
            let result = runParse "true"
            itParsesSuccessfully result
            it "parses to true" $
                result `shouldSatisfy` (\ (Right (JBool _ b)) -> b)

        describe "false" $ do
            let result = runParse "false"
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
                let result = runParse "0"
                itParsesToNumber 0 result

            context "unsigned" $ do
                let result = runParse "12345"
                itParsesToNumber 12345 result

            context "unary minus" $ do
                let result = runParse "-12345"
                itParsesToNumber (-12345) result

            context "positive exponent" $ do
                let result = runParse "12345e+1"
                itParsesToNumber 12345e+1 result

            context "negative exponent" $ do
                let result = runParse "12345e-1"
                itParsesToNumber 12345e-1 result

            context "leading zero" $ do
                let result = runParse "0123"
                itFailsToParse result

            context "unary plus" $ do
                let result = runParse "+12345"
                itFailsToParse result


        describe "decimals" $ do

            context "zero" $ do
                let result = runParse "0.0"
                itParsesToNumber 0.0 result

            context "unsigned" $ do
                let result = runParse "123.45"
                itParsesToNumber 123.45 result

            context "unary minus" $ do
                let result = runParse "-123.45"
                itParsesToNumber (-123.45) result

            context "positive exponent" $ do
                let result = runParse "123.45e+1"
                itParsesToNumber 123.45e+1 result

            context "negative exponent" $ do
                let result = runParse "123.45e-1"
                itParsesToNumber 123.45e-1 result

            context "leading zero" $ do
                let result = runParse "01.23"
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
            let result = runParse "\"hello\""
            itParsesToString "hello" result

        context "alphanumeric string" $ do
            let result = runParse "\"hello 123\""
            itParsesToString "hello 123" result

        context "escape sequences" $ do
            let result = runParse "\"hello\nworld\""
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
            let result = runParse "[]"
            itParsesToArray [] result

        context "empty array with internal padding" $ do
            let result = runParse "[  ]"
            itParsesToArray [] result

        context "singleton array" $ do
            let result = runParse "[1]"
            itParsesToArray [JNumber () 1] result

        context "heterogenous array" $ do
            let result = runParse "[true, null, false]"
            itParsesToArray [JBool () True , JNull (), JBool () False] result

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
            let result = runParse "{}"
            itParsesToObject mempty result

        context "empty object with internal padding" $ do
            let result = runParse "{  }"
            itParsesToObject mempty result

        context "singleton object" $ do
            let result = runParse "{\"foo\":\"bar\"}"
                expected = Map.fromList [ (Key () "foo", JString () "bar")]
            itParsesToObject expected result

        context "heterogenous object" $ do
            let result = runParse "{\"foo\":\"bar\", \"baz\": null}"
                expected = Map.fromList
                           [ (Key () "foo", JString () "bar")
                           , (Key () "baz", JNull ())
                           ]
            itParsesToObject expected result

    describe "parsing a twitter timeline response" $ do
        let path = "resources/twitter-timeline.json"
        file <- runIO (Paths.getDataFileName path)
        json <- runIO (BS.readFile file)
        let result = runParse json
        itParsesSuccessfully result
