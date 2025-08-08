{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase        #-}

module Bound.NestedDT2 where

import           Control.Monad
import           Control.Monad.Trans
import           Data.String
import           Text.Printf

data Var b a = B b | F a
  deriving (Eq, Ord, Read, Functor, Foldable, Traversable)

instance Show a => Show (Var b a) where
  show (B _) = "Z"
  show (F x) = "S" ++ show x

instance Monad (Var b) where
  B b >>= _ = B b
  F a >>= f = f a

instance Applicative (Var b) where
  pure = F
  (B b) <*> _     = B b
  (F _) <*> (B b) = B b
  (F f) <*> (F a) = F (f a)

newtype Scope b f a = Scope {runScope :: f (Var b (f a))}
  deriving (Functor, Foldable, Traversable)

instance Monad m => Monad (Scope b m) where
  (Scope e) >>= f = Scope $ e >>= \case
    B b -> return (B b)
    F ea -> ea >>= runScope . f

instance Applicative m => Applicative (Scope b m) where
  pure = Scope . pure . F . pure
  (Scope mf) <*> (Scope ma) = Scope $ fmap ((<*>) . fmap (<*>)) mf <*> ma

instance MonadTrans (Scope b) where
  lift = Scope . return . F

abstract :: Monad m => (a -> Maybe b) -> m a -> Scope b m a
abstract f = Scope . fmap \y -> case f y of
  Just z  -> B z
  Nothing -> F (return y)

instantiate :: Monad m => (b -> m a) -> Scope b m a -> m a
instantiate k (Scope e) = e >>= \case
  B b -> k b
  F a -> a





data Exp a
  = Var a
  | App (Exp a) (Exp a)
  | Lam (Scope () Exp a)
  deriving (Functor, Foldable, Traversable)

(@) :: Exp a -> Exp a -> Exp a
(@) = App

instance Show a => Show (Exp a) where
  show (Var a)         = show a
  show (App f g)       = printf "(%s ∙ %s)" (show f) (show g)
  show (Lam (Scope e)) = printf "λ.%s" (show e)

instance Monad Exp where
  Var a >>= f   = f a
  App l r >>= f = App (l >>= f) (r >>= f)
  Lam xs >>= f  = Lam $ xs >>= lift . f

instance Applicative Exp where
  pure = Var
  (<*>) = ap

lam :: Eq a => a -> Exp a -> Exp a
lam x b = Lam $ abstract (\y -> guard (y == x)) b

reduce :: Exp a -> Exp a
reduce (App (Lam b) a) = reduce (instantiate (const $ reduce a) b)
reduce (App f a)       = reduce (App (reduce f) a)
reduce (Var a)         = Var a
reduce (Lam b)         = Lam b

instance IsString a => IsString (Exp a) where
  fromString = Var . fromString
