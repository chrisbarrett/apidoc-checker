{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
-- |Implements verification of apidoc specs.
module Apidoc.Check
  (
    module Exports
  , validate
  ) where

import           Apidoc.Check.Env     as Exports (Env)
import           Apidoc.Check.Err     as Exports (Err (..))
import qualified Apidoc.Check.Err     as Err
import           Apidoc.Check.Lenses  as Exports
import qualified Apidoc.DSL           as DSL
import           Apidoc.Json
import           Apidoc.Pos
import           Control.Lens
import           Control.Monad.Reader
import qualified Control.Monad.Reader as Reader
import           Control.Monad.State  (MonadState, State)
import qualified Control.Monad.State  as State
import qualified Data.Foldable        as Foldable
import qualified Data.List            as List
import           Data.Map             (Map)
import qualified Data.Map.Strict      as Map
import qualified Data.Maybe           as Maybe
import           Data.Sequence        (Seq)
import qualified Data.Sequence        as Seq
import qualified Data.Sequence.Lens   as Seq
import           Data.Set             (Set)
import qualified Data.Set             as Set
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Data.Traversable
import qualified Network.URI          as URI


type Check m = (Functor m, Applicative m, Monad m, MonadState Env m, MonadPlus m)
type CheckObject m a = ReaderT Object m a


validate :: Env -> Json -> (Seq Err, Maybe DSL.Spec)
validate env =
    flip State.evalStateT env $ undefined


spec :: Check m => Json -> m DSL.Spec
spec = object $ do
    assertExpectedKeys [ "apidoc"
                       , "attributes"
                       , "base_url"
                       , "description"
                       , "enums"
                       , "headers"
                       , "imports"
                       , "info"
                       , "models"
                       , "name"
                       , "namespace"
                       , "resources"
                       , "unions"
                       ]
    DSL.Spec
        <$> optional "apidoc" apidoc
        <*> optional "attributes" (array attribute)
        <*> optional "base_url" url
        <*> optional "description" jstring
        <*> optional "enums" enums
        <*> optional "headers" headers
        <*> optional "imports" imports
        <*> optional "info" info
        <*> optional "models" models
        <*> required "name" name
        <*> optional "namespace" namespace
        <*> optional "resources" resources
        <*> optional "unions" unions


apidoc :: Check m => Json -> m DSL.Apidoc
apidoc = object $ do
    assertExpectedKeys ["version"]
    DSL.Apidoc
      <$> required "version" version
  where
    version = undefined


attribute :: Check m => Json -> m DSL.Attribute
attribute = object $ do
    assertExpectedKeys ["name", "value"]
    DSL.Attribute
      <$> required "name" jstring
      <*> required "value" anyJson


url :: Check m => Json -> m DSL.Uri
url js = do
    s <- jstring js
    maybe (invalidUri js s)
          (pure . DSL.Uri)
          (URI.parseURI (Text.unpack s))
  where
    invalidUri js s =
        pushErr (jsonPos js) (Err.InvalidUri s)


enums :: Check m => Json -> m (Map DSL.TypeName DSL.Enum)
enums = undefined


headers :: Check m => Json -> m [DSL.Header]
headers = undefined


imports :: Check m => Json -> m [DSL.Import]
imports = undefined


info :: Check m => Json -> m DSL.Info
info = undefined


models :: Check m => Json -> m (Map DSL.TypeName DSL.Model)
models = undefined


name :: Check m => Json -> m DSL.ServiceName
name js = do
    DSL.ServiceName <$> jstring js


namespace :: Check m => Json -> m DSL.Namespace
namespace js =
    DSL.Namespace <$> jstring js


resources :: Check m => Json -> m (Map DSL.TypeRef DSL.Resource)
resources = undefined


unions :: Check m => Json -> m (Map DSL.TypeName DSL.Union)
unions = undefined


anyJson :: Check m => Json -> m Json
anyJson = pure


jstring :: Check m => Json -> m Text

jstring (JString _ s) =
    pure s

jstring js =
    typeError (jsonPos js) (Err.Expected "string") (Err.Actual (typeOf js))


typeError :: Check m => Pos -> Err.Expected -> Err.Actual -> m a
typeError p expected actual = do
    pushErr p (Err.TypeError expected actual)


pushErr :: Check m => Pos -> Err.ErrType -> m a
pushErr p e = do
    envErrs <%= (`snoc` (Err p e))
    mzero


-- |Parse the given 'Json' value to an object, then apply a validation function.
-- Push errors into the context if there are duplicate keys.
object :: Check m => CheckObject m a -> Json -> m a
object f (JObject _ obj) = do
    let keys = obj ^.. objectContent.traverse._1
    checkNoDuplicates keys
    Reader.runReaderT f obj
  where
    checkNoDuplicates ks =
        fmap (\k -> (k ^. keyLabel, Seq.singleton k)) ks
          & Map.fromListWith mappend
          & Map.filter ((>) 1 . length)
          & Map.map Seq.sort
          & Map.elems
          & traverse dupKeyErr

    dupKeyErr (decl :< dups) =
        for dups (\dup ->
                  pushErr (dup ^. keyPos) (Err.DuplicateKey decl dup))
    dupKeyErr _Empty = pure mempty

object _ js =
    typeError (jsonPos js) (Err.Expected "object") (Err.Actual (typeOf js))


-- |Parse the given 'Json' value to an array, then apply a validation function.
array :: Check m => (Json -> m a) -> Json -> m [a]
array f (JArray _ xs) =
    traverse f xs
array _ js =
    typeError (jsonPos js) (Err.Expected "array") (Err.Actual (typeOf js))


-- TODO: Use 'optional' and 'required' to add the list of expected keys into the
-- context, then validate those keys automatically inside the 'object' parser.

-- |Parse a required attribute on a JSON object. If there multiple declarations
-- under the same key, validate all of them and return the larequired :: Text -> Object -> (Json -> Check a) -> Check a
required :: Check m => Text -> (Json -> m a) -> CheckObject m a
required k f = do
    obj <- Reader.ask
    obj ^. objectContent
      & Seq.filter ((==) k . view (_1.keyLabel))
      & fmap snd
      & validateAllEntries obj
  where
    -- If there multiple declarations under the same key, validate all of
    -- them and return the last.

    validateAllEntries :: Check m => Object -> Seq Json -> CheckObject m a
    validateAllEntries _ (xs :> x) = do
        traverse f xs
        pure (runReaderT (pure f) x)

    validateAllEntries o _Empty =
      pushErr (o ^. objectPos) (Err.RequiredKeyMissing k)


-- |Parse an optional attribute on a JSON object. If there multiple declarations
-- under the same key, validate all of them and return the last.
optional :: Check m => Text -> (Json -> m a) -> CheckObject m (Maybe a)
optional k f = do
    obj <- Reader.ask
    obj ^. objectContent
      & Seq.filter ((==) k . view (_1.keyLabel))
      & fmap snd
      & validateAllEntries
  where
    validateAllEntries :: Check m => Seq Json -> CheckObject m (Maybe a)
    validateAllEntries (xs :> x) = do
        traverse f xs
        Just <$> (f x)

    validateAllEntries _Empty =
        pure Nothing


assertExpectedKeys :: Check m => Set Text -> CheckObject m ()
assertExpectedKeys expectedKeys = do
    obj <- Reader.ask
    let keys = obj ^.. objectContent.traverse._1
    for keys assertInExpected
    pure ()
  where
    assertInExpected (Key p k) =
        if k `notElem` expectedKeys
        then unexpectedKey p k
        else pure ()

    unexpectedKey p k = do
        pushErr p (Err.UnexpectedKey k)
