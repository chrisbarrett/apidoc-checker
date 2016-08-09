{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedStrings          #-}
module Apidoc.Check.DSL where

import           Apidoc.Check.Err
import qualified Apidoc.Check.Parsers           as Parsers
import qualified Apidoc.DSL                     as DSL
import           Apidoc.Json                    hiding (parseNumber)
import           Apidoc.Pos                     (Pos)
import           Control.Applicative.Validation as V
import           Control.Lens                   hiding ((:<), (:>))
import           Control.Monad.Reader           (MonadReader, ReaderT)
import qualified Control.Monad.Reader           as Reader
import           Control.Monad.Writer           (MonadWriter, WriterT)
import qualified Control.Monad.Writer           as Writer
import           Data.Foldable
import           Data.Map                       (Map)
import qualified Data.Map.Strict                as Map
import           Data.Sequence                  (Seq, ViewL (..), ViewR (..))
import qualified Data.Sequence                  as Seq
import           Data.Set                       (Set)
import           Data.Text                      (Text)
import qualified Data.Text                      as Text
import qualified Network.URI                    as URI
import           Text.Read                      (readMaybe)

type Check a = Validation (Seq Err) a

newtype CheckObject a = CheckObject (
      ReaderT Object (WriterT (Set Text) (Validation (Seq Err))) a
    )
    deriving ( Functor
             , Applicative
             , Monad
             , MonadWriter (Set Text)
             , MonadReader Object
             , MonadValidate (Seq Err)
             )

runCheckObject :: CheckObject a -> Object -> (Check a, Set Text)
runCheckObject (CheckObject p) o =
    case runValidation (Writer.runWriterT (Reader.runReaderT p o)) of
      Right (e, (r, ks)) -> (Success e r, ks)
      Left e             -> (failure e, mempty)

typeRef :: Json -> Check DSL.TypeRef
typeRef js =
    string js `andThen` parse
  where
    parse :: Text -> Check DSL.TypeRef
    parse s = V.fromMaybe [parseErr s] (Parsers.parseTypeRef s)
    parseErr s = Err (jsonPos js) (UnparseableTypeRef s)


anyJson :: Json -> Check Json
anyJson = pure


uri :: Json -> Check DSL.Uri
uri js =
    string js `andThen` parse
  where
    parse :: Text -> Check DSL.Uri
    parse s =
        let u = DSL.Uri <$> URI.parseURI (Text.unpack s)
        in V.fromMaybe [parseErr s] u

    parseErr s = Err (jsonPos js) (InvalidUri s)


responseCode :: Json -> Check DSL.ResponseCode
responseCode (JString _ "default") = pure DSL.RespDefault
responseCode (JString p s) =
    parseNumber `andThen` validate
  where
    parseNumber =
        V.fromMaybe [Err p (UnparseableResponseCode s)]
          (readMaybe (Text.unpack s))

    validate n
      | isInteger n && n >= 200 && n <= 526
                    = success (DSL.RespInt (round n))
      | isInteger n = failure [Err p (ResponseCodeOutOfRange (round n))]
      | otherwise   = typeError p (Expected "HTTP response code") (Actual "float")

responseCode js =
    typeError (jsonPos js) (Expected "HTTP response code") (Actual (typeOf js))


isInteger :: Double -> Bool
isInteger d =
    d == fromIntegral (round d :: Integer)


httpMethod :: Json -> Check DSL.HttpMethod
httpMethod js =
    string js `andThen` parse
  where
    parse :: Text -> Check DSL.HttpMethod
    parse s = V.fromMaybe [parseErr s] (readMaybe (Text.unpack s))
    parseErr s = Err (jsonPos js) (InvalidHttpMethod s)


paramLocation :: Json -> Check DSL.ParameterLocation
paramLocation js =
    string js `andThen` parse
  where
    parse :: Text -> Check DSL.ParameterLocation
    parse s = V.fromMaybe [parseErr s](readMaybe (Text.unpack s))
    parseErr s = Err (jsonPos js) (InvalidParameterLocation s)


string :: Json -> Check Text
string (JString _ s) = pure s
string js =
    typeError (jsonPos js) (Expected "string") (Actual (typeOf js))


bool :: Json -> Check Bool
bool (JBool _ b) = pure b
bool js =
    typeError (jsonPos js) (Expected "bool") (Actual (typeOf js))


int :: Json -> Check Integer
int (JNumber p n)
    | isInteger n = pure (round n)
    | otherwise   = typeError p (Expected "integer") (Actual "float")
int js =
    typeError (jsonPos js) (Expected "integer") (Actual (typeOf js))

-- |Parse the given 'Json' value to an object, then apply a validation function.
-- Push errors into the context if there are duplicate keys.
object :: CheckObject a -> Json -> Check a
object f (JObject _ obj) =
    let keys = obj ^.. objectContent.traverse._1
    in checkNoDuplicates keys *> validate keys
  where
    validate keys =
        let (parser, expectedKeys) = runCheckObject f obj
        in parser `andThen` \r ->
            traverse_ (checkInExpected expectedKeys) keys *> pure r

    checkInExpected :: Set Text -> Key -> Check ()
    checkInExpected ks (Key p k)
        | k `elem` ks = success ()
        | otherwise   = failure [Err p (UnexpectedKey k)]

    checkNoDuplicates :: [Key] -> Check ()
    checkNoDuplicates ks =
        fmap (\k -> (k ^. keyLabel, Seq.singleton k)) ks
          & Map.fromListWith mappend
          & Map.filter ((>) 1 . length)
          & Map.map Seq.sort
          & Map.elems
          & traverse_ (warnOnDups . Seq.viewl)

    warnOnDups :: ViewL Key -> Check ()
    warnOnDups EmptyL         = pure mempty
    warnOnDups (decl :< dups) =
        for_ dups $ \dup ->
          warning [Err (dup ^. keyPos) (DuplicateKey decl dup)] dup

object _ js =
    typeError (jsonPos js) (Expected "object") (Actual (typeOf js))


-- |Parse the given 'Json' value to a map, applying validation functions over
-- the keys and values.
mapOf :: Ord k => (Key -> Check k) -> (Json -> Check v) -> Json -> Check (Map k v)
mapOf pk pv (JObject _ obj) =
    obj ^. objectContent
    & fmap validateKvp
    & sequenceA
    & fmap (Map.fromList . toList)
  where
    validateKvp (k, v) =
        (,) <$> pk k <*> pv v

mapOf _ _ js =
    typeError (jsonPos js) (Expected "object") (Actual (typeOf js))


-- |Lift a Json parser into a key parser for use with 'mapOf'.
key :: (Json -> Check a) -> Key -> Check a
key f k =
    let p = k ^. keyPos
        s = JString p (k ^. keyLabel)
    in
      f s


-- |Parse the given 'Json' value to an array, then apply a validation function.
array :: (Json -> Check a) -> Json -> Check (Seq a)
array f (JArray _ xs) = traverse f xs
array _ js =
    typeError (jsonPos js) (Expected "array") (Actual (typeOf js))


-- |Parse a required attribute on a JSON object. If there are multiple
-- declarations under the same key, validate all of them and return the last.
required :: Text -> (Json -> Check a) -> CheckObject a
required k f = do
    o <- Reader.ask
    r <- optional k f
    case r of
      Just res -> pure res
      Nothing -> failure [Err (o ^. objectPos) (RequiredKeyMissing k)]


-- |Parse an optional attribute on a JSON object. If there are multiple
-- declarations under the same key, validate all of them and return the last.
optional :: Text -> (Json -> Check a) -> CheckObject (Maybe a)
optional k f = do
    Writer.tell [k]
    obj <- Reader.ask
    res <-
        obj ^. objectContent
          & Seq.filter ((==) k . view (_1.keyLabel))
          & fmap snd
          & Seq.viewr
          & validateAllEntries
    case res of
      Success e r -> warning e r
      Failure e   -> failure e
  where
    validateAllEntries EmptyR    = pure (success Nothing)
    validateAllEntries (es :> e) = pure (traverse_ f es *> fmap Just (f e))


typeError :: Pos -> Expected -> Actual -> Check a
typeError p expected actual =
    failure [Err p (TypeError expected actual)]
