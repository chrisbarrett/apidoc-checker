{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE UndecidableInstances   #-}
module Control.Applicative.Validation where

import           Control.Applicative
import           Control.Monad        (liftM)
import           Control.Monad.Reader (ReaderT)
import           Control.Monad.State  (StateT)
import           Control.Monad.Trans  (MonadTrans, lift)
import           Control.Monad.Writer (WriterT)
import           Data.Monoid          ((<>))


-- * Validation type

-- |Accumulate a monoidal value representing error state. The successful case
-- can accumulate errors too, allowing warnings to be collected during
-- validation.
data Validation e a where
    Success :: Monoid e => e -> a -> Validation e a
    Failure :: Monoid e => e -> Validation e a

deriving instance (Show e, Show a) => Show (Validation e a)

deriving instance (Eq e, Eq a) => Eq (Validation e a)

deriving instance (Ord e, Ord a) => Ord (Validation e a)

deriving instance Functor (Validation e)

instance Foldable (Validation e) where
    foldr f x (Success _ a) = f a x
    foldr _ x (Failure _)   = x

instance Traversable (Validation e) where
    traverse f (Success e a) = Success e <$> f a
    traverse _ (Failure e)   = pure (Failure e)

instance (Monoid e, Monoid a) => Monoid (Validation e a) where
    mempty = Success mempty mempty
    mappend (Success el vl) (Success er vr) = Success (el <> er) (vl <> vr)
    mappend (Failure el)    (Failure er)    = Failure (el <> er)
    mappend (Success el _)  (Failure er)    = Failure (el <> er)
    mappend (Failure el)    (Success er _)  = Failure (el <> er)

instance Monoid e => Applicative (Validation e) where
    pure = Success mempty
    Failure l   <*> Failure r   = Failure (l <> r)
    Failure l   <*> Success r _ = Failure (l <> r)
    Success l _ <*> Failure r   = Failure (l <> r)
    Success l f <*> Success r a = Success (l <> r) (f a)

instance Monoid e => Monad (Validation e) where
    l@(Success _ a) >>= f = l *> f a
    Failure e       >>= _ = Failure e

-- * Validation monad transformer

data ValidationT e m a = ValidationT {runValidationT :: m (Validation e a)}
    deriving (Functor)

instance (Monoid e, Applicative m) => Applicative (ValidationT e m) where
    pure = ValidationT . pure . pure
    ValidationT f <*> ValidationT m = ValidationT (liftA2 (<*>) f m)

instance Foldable m => Foldable (ValidationT e m) where
    foldr f x (ValidationT m) = foldr (flip (foldr f)) x m

instance Traversable m => Traversable (ValidationT e m) where
    traverse f (ValidationT m) = ValidationT <$> traverse (traverse f) m

instance (Monoid e, Monad m) => Monad (ValidationT e m) where
    ValidationT m >>= f =
        ValidationT (m >>= \case
                              Success _ a -> runValidationT (f a)
                              Failure e -> pure (Failure e))

instance Monoid e => MonadTrans (ValidationT e) where
    lift = ValidationT . liftM (Success mempty)


-- * MonadValidate class

class (Monoid e, Monad m) => MonadValidate e m | m -> e where
    success :: a -> m a
    warning :: e -> a -> m a
    failure :: e -> m a

instance Monoid e => MonadValidate e (Validation e) where
    success a = pure a
    warning e a = Success e a
    failure e = Failure e

instance (Monoid e, Monad m) => MonadValidate e (ValidationT e m) where
    success a = ValidationT (pure (success a))
    warning e a = ValidationT (pure (warning e a))
    failure e = ValidationT (pure (failure e))

instance (Monoid e, MonadValidate e m) => MonadValidate e (ReaderT r m) where
    success = lift . success
    warning e a = lift (warning e a)
    failure = lift . failure

instance (Monoid s, Monoid e, MonadValidate e m) => MonadValidate e (WriterT s m) where
    success = lift . success
    warning e a = lift (warning e a)
    failure = lift . failure

instance (Monoid s, Monoid e, MonadValidate e m) => MonadValidate e (StateT s m) where
    success = lift . success
    warning e a = lift (warning e a)
    failure = lift . failure



-- * Utility functions

runValidation :: Validation e a -> Either e (e, a)
runValidation (Failure e)   = Left e
runValidation (Success e r) = Right (e, r)

fromMaybe :: (Monoid e, MonadValidate e m) => e -> Maybe a -> m a
fromMaybe e Nothing  = failure e
fromMaybe _ (Just a) = success a

andThen :: Validation e a -> (a -> Validation e b) -> Validation e b
andThen l@(Success _ a) f = l *> f a
andThen (Failure e)     _ = Failure e
