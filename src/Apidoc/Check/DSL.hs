{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ViewPatterns               #-}
module Apidoc.Check.DSL where

import           Apidoc.Check.Err
import qualified Apidoc.Check.Parsers as Parsers
import qualified Apidoc.DSL           as DSL
import           Apidoc.Json          hiding (parseNumber)
import           Apidoc.Pos           (Pos)
import           Control.Lens         hiding ((:<), (:>))
import           Control.Monad.Reader (MonadReader, ReaderT)
import qualified Control.Monad.Reader as Reader
import           Control.Monad.Writer (MonadWriter, WriterT)
import qualified Control.Monad.Writer as Writer
import           Data.Foldable
import qualified Data.List            as List
import           Data.Map             (Map)
import qualified Data.Map.Strict      as Map
import qualified Data.Maybe           as Maybe
import           Data.Sequence        (Seq, ViewL (..), ViewR (..))
import qualified Data.Sequence        as Seq
import           Data.Set             (Set)
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Data.Validation
import qualified Network.URI          as URI
import qualified Text.EditDistance    as EditDistance
import           Text.Read            (readMaybe)

-- * Validation transformer stack

type Check a = AccValidation (Seq Err) a

-- HACK: Frankenvalidator using Validation's monad instance with AccValidation's
-- applicative instance.
newtype AccValidationM e a = AccValidationM {unAccValidationM :: AccValidation e a}
    deriving (Functor, Applicative, Traversable, Foldable)

instance Monad (AccValidationM (Seq Err)) where
    AccValidationM (AccFailure err) >>= _ = AccValidationM (AccFailure err)
    AccValidationM (AccSuccess r)   >>= f = f r


instance Validate AccValidationM where
    _Validation =
        iso (view _Validation . unAccValidationM)
            (AccValidationM . view _AccValidation)
    _AccValidation =
        iso (view _AccValidation . unAccValidationM)
            (AccValidationM . view _AccValidation)
    _Either =
        iso (view _Either . unAccValidationM)
            (AccValidationM . view _AccValidation)

newtype CheckObject e a = CheckObject {
        unCheckObject :: ReaderT Object
                                 (WriterT (Set Text) (AccValidationM (Seq Err)))
                                 a
    }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadWriter (Set Text)
             , MonadReader Object
             )

type CheckObject' a = CheckObject (Seq Err) a

runCheckObject :: CheckObject (Seq Err) a -> Object -> (Check a, Set Text)
runCheckObject (CheckObject p) o =
    case unAccValidationM (Writer.runWriterT (Reader.runReaderT p o)) of
      AccSuccess (r, ks) -> (_Success # r, ks)
      AccFailure e       -> (_Failure # e, mempty)


-- Unfortunately, 'CheckObject' is not isomorphic to the other validation
-- classes.

successCheckObject :: a -> CheckObject' a
successCheckObject x = CheckObject (Reader.ReaderT (const (Writer.WriterT (_Success # (x, mempty)))))

failureCheckObject :: Seq Err -> CheckObject' a
failureCheckObject e = CheckObject (Reader.ReaderT (const (Writer.WriterT (_Failure # e))))


-- * Validation DSL implementation

fromMaybe :: Validate v => e -> Maybe a -> v e a
fromMaybe e Nothing = _Failure # e
fromMaybe _ (Just x) = _Success # x

typeRef :: Json -> Check DSL.TypeRef
typeRef js =
    unAccValidationM (AccValidationM (string js) >>= parse)
  where
    parse s = fromMaybe [err s] (Parsers.parseTypeRef s)
    err s = Err (jsonPos js) (UnparseableTypeRef s)


anyJson :: Json -> Check Json
anyJson = pure


uri :: Json -> Check DSL.Uri
uri js =
    unAccValidationM (AccValidationM (string js) >>= parse)
  where
    parse s = fromMaybe [err s] (DSL.Uri <$> URI.parseURI (Text.unpack s))
    err s = Err (jsonPos js) (InvalidUri s)


responseCode :: Json -> Check DSL.ResponseCode
responseCode (JString _ "default") = pure DSL.RespDefault
responseCode (JString p str) = unAccValidationM $ do
    number <- parseNumber str
    validate number
  where
    parseNumber s = fromMaybe [err s] (readMaybe (Text.unpack s))
    err s = Err p (UnparseableResponseCode s)

    validate :: Double -> AccValidationM (Seq Err) DSL.ResponseCode
    validate n
      | isInteger n && n >= 200 && n <= 526
                    = _Success # (DSL.RespInt (round n))
      | isInteger n = _Failure # [Err p (ResponseCodeOutOfRange (round n))]
      | otherwise   = typeError p (Expected "HTTP response code") (Actual "float")

responseCode js =
    typeError (jsonPos js) (Expected "HTTP response code") (Actual (typeOf js))


isInteger :: Double -> Bool
isInteger d =
    d == fromIntegral (round d :: Integer)


httpMethod :: Json -> Check DSL.HttpMethod
httpMethod js =
    unAccValidationM (AccValidationM (string js) >>= parse)
  where
    parse s = fromMaybe [err s] (readMaybe (Text.unpack s))
    err s = Err (jsonPos js) (InvalidHttpMethod s)


paramLocation :: Json -> Check DSL.ParameterLocation
paramLocation js =
    unAccValidationM (AccValidationM (string js) >>= parse)
  where
    parse s =
        case s of
          "path"  -> _Success # DSL.Path
          "query" -> _Success # DSL.Query
          "form"  -> _Success # DSL.Form
          _       -> _Failure # [Err (jsonPos js) (InvalidParameterLocation s)]


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
object :: CheckObject' a -> Json -> Check a
object f (JObject _ obj) =
    let keys = obj ^.. objectContent.traverse._1
    in checkNoDuplicates keys *> validate keys
  where
    validate keys =
        unAccValidationM $
        let (parser, expectedKeys) = runCheckObject f obj
        in do
            r <- AccValidationM parser
            traverse_ (checkInExpected expectedKeys) keys *> pure r

    checkInExpected ks (Key p k)
        | k `elem` ks = _Success # ()
        | otherwise   = _Failure # [Err p (UnexpectedKey k (suggestion k ks))]

    checkNoDuplicates :: [Key] -> Check ()
    checkNoDuplicates ks =
        fmap (\k -> (k ^. keyLabel, Seq.singleton k)) ks
          & Map.fromListWith mappend
          & Map.filter ((>) 1 . length)
          & Map.map Seq.sort
          & Map.elems
          & traverse_ (errOnDups . Seq.viewl)

    errOnDups :: ViewL Key -> Check ()
    errOnDups EmptyL         = pure mempty
    errOnDups (decl :< dups) =
        for_ dups $ \dup ->
          _Failure # [Err (dup ^. keyPos) (DuplicateKey decl dup)]

object _ js =
    typeError (jsonPos js) (Expected "object") (Actual (typeOf js))


suggestion :: Foldable f => Text -> f Text -> Maybe Text
suggestion t ks =
    let distances = fmap (\s -> (distance t s, s)) (toList ks)
        best = Maybe.listToMaybe $ List.sortOn fst distances
    in case best of
         Just (score, c) | score < maximumEditDistance -> Just c
         _ -> Nothing
  where
    maximumEditDistance = 5

    distance (Text.unpack -> t1) (Text.unpack -> t2) =
        EditDistance.restrictedDamerauLevenshteinDistance EditDistance.defaultEditCosts t1 t2

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
required :: Text -> (Json -> Check a) -> CheckObject' a
required k f = do
    o <- Reader.ask
    r <- optional k f
    case r of
      Just x  -> successCheckObject x
      Nothing -> failureCheckObject [Err (o ^. objectPos) (RequiredKeyMissing k)]


-- |Parse an optional attribute on a JSON object. If there are multiple
-- declarations under the same key, validate all of them and return the last.
optional :: Text -> (Json -> Check a) -> CheckObject' (Maybe a)
optional k f = do
    Writer.tell [k]
    obj <- Reader.ask
    let entries = obj ^. objectContent
                    & Seq.filter ((==) k . view (_1.keyLabel))
                    & fmap snd
                    & Seq.viewr
    case entries of
      EmptyR -> successCheckObject Nothing
      xs :> x ->
          pure (traverse f xs) *>
            case f x of
              AccSuccess r -> successCheckObject (Just r)
              AccFailure e -> failureCheckObject e

typeError :: Validate v => Pos -> Expected -> Actual -> v (Seq Err) a
typeError p expected actual =
    _Failure # [Err p (TypeError expected actual)]
