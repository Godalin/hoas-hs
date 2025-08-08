module AEnH where

-- |
-- * Algebraic Effect and Handlers

type Name = String

data Value
  = Vtru | Vfls
  | Vfun (Value -> Computation)
  | Vhdl (Value -> Computation) [(Name, Value -> Value -> Computation)]

data Computation
  = Cret Value
  | Cop Name Value (Value -> Computation)
  | Cbnd Computation (Value -> Computation)
  | Cif Value Computation Computation
  | Capp Value Value
  | Chdl Value Computation

(@) :: Value -> Value -> Computation
(@) = Capp
