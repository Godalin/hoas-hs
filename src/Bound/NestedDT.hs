{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE LambdaCase        #-}

module Bound.NestedDT where

import           Control.Monad
import           Data.String

data Exp a
  = Var a
  | App (Exp a) (Exp a)
  | Lam (Exp (Maybe a))
  deriving (Show, Eq, Read, Functor, Foldable, Traversable)

(@) :: Exp a -> Exp a -> Exp a
(@) = App

instance Monad Exp where
  (>>=) :: Exp a -> (a -> Exp b) -> Exp b
  Var a >>= f   = f a
  App l r >>= f = App (l >>= f) (r >>= f)
  Lam xs >>= f  = Lam $ xs >>= \case
    Nothing -> Var Nothing
    Just a -> fmap Just (f a)

instance Applicative Exp where
  pure = Var
  (<*>) = ap

abstract :: Eq a => a -> Exp a -> Exp (Maybe a)
abstract a = fmap (\x -> if x == a then Nothing else Just x)

lam :: Eq a => a -> Exp a -> Exp a
lam x b = Lam $ abstract x b

instantiate :: Exp a -> Exp (Maybe a) -> Exp a
instantiate e = (>>= \case
  Nothing -> e
  Just a  -> Var a)

reduce :: Exp a -> Exp a
reduce (App (Lam b) a) = reduce (instantiate (reduce a) b)
reduce (App f a)       = reduce (App (reduce f) a)
reduce (Var a)         = Var a
reduce (Lam b)         = Lam b

instance IsString a => IsString (Exp a) where
  fromString = Var . fromString
