{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase        #-}

module Bound.NestedDT1 where

import           Control.Monad
import           Control.Monad.Trans
import           Data.String
import           Text.Printf

newtype Scope f a = Scope {runScope :: f (Maybe a)}
  deriving (Functor, Foldable, Traversable)

instance Monad m => Monad (Scope m) where
  (Scope m) >>= f = Scope $ m >>= \case
    Nothing -> return Nothing
    Just a -> runScope $ f a

instance Applicative m => Applicative (Scope m) where
  pure = Scope . pure . Just
  (Scope mf) <*> (Scope ma) = Scope $ fmap (<*>) mf <*> ma

instance MonadTrans Scope where
  lift = Scope . fmap Just

data Exp a
  = Var a
  | App (Exp a) (Exp a)
  | Lam (Scope Exp a)
  deriving (Functor, Foldable, Traversable)

(@) :: Exp a -> Exp a -> Exp a
(@) = App

instance Show a => Show (Exp a) where
  show (Var a)         = show a
  show (App f g)       = printf "%s ∙ %s" (show f) (show g)
  show (Lam (Scope e)) = printf "λ. %s" (show e)


instance Monad Exp where
  Var a >>= f   = f a
  App l r >>= f = App (l >>= f) (r >>= f)
  Lam xs >>= f  = Lam $ xs >>= lift . f

instance Applicative Exp where
  pure = Var
  (<*>) = ap

abstract :: Eq a => a -> Exp a -> Scope Exp a
abstract x = Scope . fmap (\y -> y <$ guard (x /= y))

lam :: Eq a => a -> Exp a -> Exp a
lam x b = Lam $ abstract x b

instantiate :: Exp a -> Scope Exp a -> Exp a
instantiate e (Scope b) = b >>= \case
  Nothing -> e
  Just a  -> return a

reduce :: Exp a -> Exp a
reduce (App (Lam b) a) = reduce (instantiate (reduce a) b)
reduce (App f a)       = reduce (App (reduce f) a)
reduce (Var a)         = Var a
reduce (Lam b)         = Lam b

instance IsString a => IsString (Exp a) where
  fromString = Var . fromString
