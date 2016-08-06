{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Apidoc.Check.Internal where

import           Apidoc.Check.CheckM
import           Apidoc.Check.CheckObject
import           Apidoc.Check.Err         (Err (..))
import qualified Apidoc.Check.Err         as Err
import           Apidoc.Check.Lenses
import qualified Apidoc.DSL               as DSL
import           Apidoc.Json
import           Apidoc.Pos               (Pos)
import           Control.Lens
import           Control.Monad            (mzero)
import qualified Control.Monad.State      as State
import qualified Data.Map.Strict          as Map
import qualified Data.Sequence            as Seq
import           Data.Text                (Text)
import qualified Data.Text                as Text
import           Data.Traversable
import qualified Network.URI as URI

anyJson :: Check m => Json -> m Json
anyJson = pure

uri :: Check m => Json -> m DSL.Uri
uri js = do
    s <- jstring js
    maybe (invalidUri js s)
          (pure . DSL.Uri)
          (URI.parseURI (Text.unpack s))
  where
    invalidUri js s =
        pushErr (jsonPos js) (Err.InvalidUri s)

jstring :: Check m => Json -> m Text
jstring (JString _ s) = pure s
jstring js =
    typeError (jsonPos js) (Err.Expected "string") (Err.Actual (typeOf js))


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

    checkInExpected ks (Key p k)
        | k `elem` ks = pure ()
        | otherwise   = unexpectedKey p k

    unexpectedKey p k = do
        pushErr p (Err.UnexpectedKey k)


object _ js =
    typeError (jsonPos js) (Err.Expected "object") (Err.Actual (typeOf js))


-- |Parse the given 'Json' value to an array, then apply a validation function.
array :: Check m => (Json -> m a) -> Json -> m [a]
array f (JArray _ xs) =
    traverse f xs
array _ js =
    typeError (jsonPos js) (Err.Expected "array") (Err.Actual (typeOf js))


-- |Parse a required attribute on a JSON object. If there multiple declarations
-- under the same key, validate all of them and return the last.
required :: Check m => Text -> (Json -> m a) -> CheckObject m a
required k f = do
    res <- optional k f
    maybe keyMissingError pure res
    where
      keyMissingError = do
          o <- askObject
          liftChecker $ pushErr (o ^. objectPos) (Err.RequiredKeyMissing k)

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

typeError :: Check m => Pos -> Err.Expected -> Err.Actual -> m a
typeError p expected actual = do
    pushErr p (Err.TypeError expected actual)


pushErr :: Check m => Pos -> Err.ErrType -> m a
pushErr p e = do
    envErrs <%= (`snoc` (Err p e))
    mzero
