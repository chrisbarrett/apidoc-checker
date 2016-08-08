{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Apidoc.Check.Internal where

import           Apidoc.Check.CheckM
import           Apidoc.Check.CheckObject
import           Apidoc.Check.Err
import           Apidoc.Check.Lenses
import qualified Apidoc.Check.Parsers     as Parsers
import qualified Apidoc.DSL               as DSL
import           Apidoc.Json
import           Apidoc.Pos               (Pos)
import           Control.Lens
import           Control.Monad            (mzero)
import qualified Control.Monad.State      as State
import qualified Data.Foldable            as Foldable
import           Data.Map                 (Map)
import qualified Data.Map.Strict          as Map
import           Data.Sequence            (Seq)
import qualified Data.Sequence            as Seq
import           Data.Text                (Text)
import qualified Data.Text                as Text
import           Data.Traversable
import qualified Network.URI              as URI
import           Text.Read                (readMaybe)


typeRef :: Check m => Json -> m DSL.TypeRef
typeRef js = do
    s <- string js
    Parsers.parseTypeRef s `orElse` pushErr (jsonPos js) (UnparseableTypeRef s)

anyJson :: Check m => Json -> m Json
anyJson = pure

uri :: Check m => Json -> m DSL.Uri
uri js = do
    s <- string js
    let parsed = DSL.Uri <$> URI.parseURI (Text.unpack s)
    parsed `orElse` pushErr (jsonPos js) (InvalidUri s)


responseCode :: Check m => Json -> m DSL.ResponseCode
responseCode (JString p "default") = pure DSL.RespDefault
responseCode (JString p s) =
    readMaybe (Text.unpack s)
      & maybe (pushErr p (UnparseableResponseCode s)) parseNumber
  where
    parseNumber n
      | isInteger n && n >= 200 && n <= 526 = pure (DSL.RespInt (round n))
      | isInteger n = pushErr p (ResponseCodeOutOfRange (round n))
      | otherwise = typeError p (Expected "HTTP response code") (Actual "float")
responseCode js =
    typeError (jsonPos js) (Expected "HTTP response code") (Actual (typeOf js))


isInteger :: Double -> Bool
isInteger d =
    d == fromIntegral (round d)


httpMethod :: Check m => Json -> m DSL.HttpMethod
httpMethod js = do
    method <- string js
    readMaybe (Text.unpack method) `orElse` pushErr (jsonPos js) (InvalidHttpMethod method)


paramLocation :: Check m => Json -> m DSL.ParameterLocation
paramLocation js = do
    loc <- string js
    readMaybe (Text.unpack loc) `orElse` pushErr (jsonPos js) (InvalidParameterLocation loc)


string :: Check m => Json -> m Text
string (JString _ s) = pure s
string js =
    typeError (jsonPos js) (Expected "string") (Actual (typeOf js))


bool :: Check m => Json -> m Bool
bool (JBool _ b) = pure b
bool js =
    typeError (jsonPos js) (Expected "bool") (Actual (typeOf js))


int :: Check m => Json -> m Integer
int (JNumber p n)
    | isInteger n = pure (round n)
    | otherwise   = typeError p (Expected "integer") (Actual "float")
int js =
    typeError (jsonPos js) (Expected "integer") (Actual (typeOf js))


-- |Parse the given 'Json' value to an object, then apply a validation function.
-- Push errors into the context if there are duplicate keys.
object :: Check m => CheckObject m a -> Json -> m a
object f (JObject _ obj) = do
    let keys = obj ^.. objectContent.traverse._1
    checkNoDuplicates keys
    (res, expectedKeys) <- runCheckObject f obj
    for keys (checkInExpected expectedKeys)
    pure res
  where
    checkInExpected ks (Key p k)
        | k `elem` ks = pure ()
        | otherwise   = unexpectedKey p k

    unexpectedKey p k = do
        pushErr p (UnexpectedKey k)


object _ js =
    typeError (jsonPos js) (Expected "object") (Actual (typeOf js))


checkNoDuplicates :: Check m => [Key] -> m ()
checkNoDuplicates ks =
    fmap (\k -> (k ^. keyLabel, Seq.singleton k)) ks
      & Map.fromListWith mappend
      & Map.filter ((>) 1 . length)
      & Map.map Seq.sort
      & Map.elems
      & Foldable.traverse_ dupKeyErr
  where

    dupKeyErr (decl :< dups) =
        for dups (\dup ->
                  pushErr (dup ^. keyPos) (DuplicateKey decl dup))
    dupKeyErr _Empty = pure mempty


-- |Parse the given 'Json' value to a map, applying validation functions over
-- the keys and values.
jmap :: (Ord k, Check m) => (Key -> m k) -> (Json -> m v) -> Json -> m (Map k v)
jmap pk pv (JObject _ obj) =
    obj ^. objectContent
    & fmap validateKvp
    & sequence
    & fmap (Map.fromList . Foldable.toList)
  where
    validateKvp (k, v) =
        (,) <$> pk k <*> pv v

jmap _ _ js =
    typeError (jsonPos js) (Expected "object") (Actual (typeOf js))


-- |Lift a Json parser into a key parser for use with 'jmap'.
key :: Check m => (Json -> m a) -> Key -> m a
key f k =
    let p = k ^. keyPos
        s = JString p (k ^. keyLabel)
    in
      f s


-- |Parse the given 'Json' value to an array, then apply a validation function.
array :: Check m => (Json -> m a) -> Json -> m (Seq a)
array f (JArray _ xs) =
    traverse f xs
array _ js =
    typeError (jsonPos js) (Expected "array") (Actual (typeOf js))


-- |Parse a required attribute on a JSON object. If there multiple declarations
-- under the same key, validate all of them and return the last.
required :: Check m => Text -> (Json -> m a) -> CheckObject m a
required k f = do
    res <- optional k f
    res `orElse` keyMissingError
    where
      keyMissingError = do
          o <- askObject
          liftChecker $ pushErr (o ^. objectPos) (RequiredKeyMissing k)

-- |Parse an optional attribute on a JSON object. If there multiple declarations
-- under the same key, validate all of them and return the last.
optional :: Check m => Text -> (Json -> m a) -> CheckObject m (Maybe a)
optional k f = do
    pushExpectedKey k
    obj <- askObject
    obj ^. objectContent
      & Seq.filter ((==) k . view (_1.keyLabel))
      & fmap snd
      & validateAllEntries f
  where
    validateAllEntries validate (xs :> x) = do
        traverse (liftChecker . validate) xs
        Just <$> (liftChecker (validate x))

    validateAllEntries _Empty _ =
        pure Nothing


liftChecker :: Check m => m a -> CheckObject m a
liftChecker f =
    CheckObject $ State.StateT (\s -> (,) <$> f
                                          <*> pure s)

-- * Error reporting

typeError :: Check m => Pos -> Expected -> Actual -> m a
typeError p expected actual = do
    pushErr p (TypeError expected actual)


pushErr :: Check m => Pos -> ErrType -> m a
pushErr p e = do
    envErrs <%= (`snoc` (Err p e))
    mzero

orElse :: Applicative f => Maybe a -> f a -> f a
orElse x e = maybe e pure x
