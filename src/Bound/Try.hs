{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Bound.Try where

import           Data.String (IsString (..))

newtype Scope f a = Scope (f a)
  deriving (Show, Eq, Read, Functor, Foldable, Traversable)

data Term a
  = Var a
  | Idx !Int
  | App (Term a) (Term a)
  | Lam (Scope Term a)
  deriving (Show, Eq, Read, Functor, Foldable, Traversable)

(@) :: Term a -> Term a -> Term a
(@) = App

abstract :: Eq a => a -> Term a -> Scope Term a
abstract me expr = Scope (letmeB 0 expr) where
  letmeB this (Var you) | you == me = Idx this
                        | otherwise = Var you
  letmeB this (Idx that) = Idx that
  letmeB this (App fun arg) = App (letmeB this fun) (letmeB this arg)
  letmeB this (Lam (Scope body)) = Lam $ Scope $ letmeB (succ this) body

instantiate :: Term a -> Scope Term a -> Term a
instantiate what (Scope body) = what'sB 0 body where
  what'sB this (Idx that) | this == that = what
                          | otherwise = Idx that
  what'sB this (Var you) = Var you
  what'sB this (App fun arg) = App (what'sB this fun) (what'sB this arg)
  what'sB this (Lam (Scope body)) = Lam $ Scope $ what'sB (succ this) body

lam :: Eq a => a -> Term a -> Term a
lam x t = Lam $ abstract x t

-- | Allow using numeric literals to build de Bruijn indices directly.
--
-- With this instance, you can write for example:
--   (1 :: Term a)  -- desugars to Idx 1
-- Negative literals are rejected at runtime.
instance Num (Term a) where
  fromInteger n
    | n < 0     = error "Idx cannot be negative"
    | otherwise = Idx (fromInteger n)

  -- The following numeric operations are not defined for 'Term'.
  -- They are provided only to satisfy the 'Num' class; using them will fail at runtime.
  (+)     _ _ = error "(+) is not supported for Term; only numeric literals construct Idx"
  (*)     _ _ = error "(*) is not supported for Term; only numeric literals construct Idx"
  abs       _ = error "abs is not supported for Term; only numeric literals construct Idx"
  signum    _ = error "signum is not supported for Term; only numeric literals construct Idx"
  negate    _ = error "negate is not supported for Term; only numeric literals construct Idx"

-- | Allow using string literals to build variables directly.
-- Requires -XOverloadedStrings at use sites.
-- Example: ("x" :: Term String) desugars to Var "x".
instance IsString a => IsString (Term a) where
  fromString = Var . fromString
