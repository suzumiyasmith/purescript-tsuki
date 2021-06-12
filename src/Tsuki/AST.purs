module Tsuki.AST where

import Prelude

import Data.Tuple

import Control.Monad.Writer
import Control.Monad.State


type Name = String

data Stat 
  = Assign Exp Exp
  | LocalAssign Exp Exp
  -- | RawStat String

-- instance semigroupStat :: Semigroup Stat where
  -- append (Do b1 r1) (Do b2 r2) = Do (b1 <> b2) r2
  -- append (Do b1 r) EmptyStat = Do b1 r
  -- append (Do b1 r) s2 = Do (snoc b1 s2) r
  -- append EmptyStat s2 = s2
  -- append s1 EmptyStat = s1
  -- append s1 s2 = Do [s1, s2] Nil

-- instance monoidStat :: Monoid Stat where
  
type Block a = StateT Int (Writer (Array Stat)) a

data Exp
  = Nil
  | Bool Boolean
  | NumberInt Int
  | Number Number
  | String String
  | RawString String
  | Index Exp Exp
  | RawFile String -- load string from file path
  -- | Vararg -- ^/.../
  | Fun0 (Block Exp)
  | Fun1 (Exp -> Block Exp) -- ^/function (..) .. end/
  | Fun2 (Exp -> Exp -> Block Exp) -- ^/function (..) .. end/
  | Fun3 (Exp -> Exp -> Exp -> Block Exp) -- ^/function (..) .. end/
  | Fun4 (Exp -> Exp -> Exp -> Exp -> Block Exp) -- ^/function (..) .. end/
  | Ident Name
  | FunCall Exp (Array Exp)
  | TableConst (Array (Tuple Name Exp)) -- ^table constructor
  | ListConst (Array Exp) -- ^table constructor
  | Binop Name Exp Exp -- ^binary operators, /+ - * ^ % .. < <= > >= == ~= and or/
  | Unop Name Exp -- ^unary operators, /- not #/
  -- | RawExp String

-- data Binop = Add | Sub | Mul | Div | Exp | Mod | Concat
--     | LT | LTE | GT | GTE | EQ | NEQ | And | Or
--     | IDiv | ShiftL | ShiftR | BAnd | BOr | BXor

-- data Unop = Neg | Not | Len | Complement


instance semiringExp :: Semiring Exp where
  add = Binop "+"
  zero = NumberInt 0
  mul = Binop "*"
  one = NumberInt 1

instance ringExp :: Ring Exp where
  sub = Binop "-"

instance commuExp :: CommutativeRing Exp

instance euclideanRingExp :: EuclideanRing Exp where
  degree = const 1 -- no idea how it works
  div = Binop "//"
  mod = Binop "%"

instance heytingAlgebraExp :: HeytingAlgebra Exp where
  tt = Bool true
  ff = Bool false
  conj = Binop "and"
  disj = Binop "or"
  not = Unop "not"
  -- implies a b = (Binop "or") (Unop "not" a) b
  implies a b = not a || b

