module Tsuki where

import Prelude
import Control.Monad.Writer
import Control.Monad.State as S
import Tsuki.AST

import Foreign.Object
import Type.Row.Homogeneous

nil :: Exp
nil = Nil

ident :: Name -> Exp
ident = Ident

indext :: Exp -> Exp -> Exp
indext = Index

-- | get key from table
gett :: Exp -> Exp -> Exp
gett k t = Index k t

get :: Name -> Exp -> Exp
get k t = gett (lit k) t

geti :: Int -> Exp -> Exp
geti i t = gett (lit i) t

-- | set left exp to right exp 
set_ :: Name -> Exp -> Block Unit
set_ l r = tell [Assign (Ident l) r]

local :: Name -> Exp -> Block Unit
local l r = tell [LocalAssign (ident l) r]

newVar :: Exp -> Block Exp
newVar r = do
  l <- S.gets (\fresh -> ident $ "localvar_" <> show fresh)
  tell [LocalAssign l r]
  S.modify_ (\k -> k + 1)
  pure l

newVar_ :: Exp -> Block Unit
newVar_ r = do
  l <- S.gets (\fresh -> ident $ "unusedvar_" <> show fresh)
  tell [LocalAssign l r]
  S.modify_ (\k -> k + 1)

set :: Exp -> Name -> Exp -> Block Unit
set t k v = sett t (lit k) v

sett :: Exp -> Exp -> Exp -> Block Unit
sett t k v = tell [Assign (gett k t) v]

table :: forall r . Homogeneous r Exp => Record r -> Exp
table = fromHomogeneous >>> toUnfoldable >>> TableConst

list :: Array Exp -> Exp
list = ListConst

len :: Exp -> Exp
len = Unop "#"

equals :: Exp -> Exp -> Exp
equals = Binop "=="

gt :: Exp -> Exp -> Exp
gt = Binop ">"

lt :: Exp -> Exp -> Exp
lt = Binop "<"

ifte :: Exp -> Exp -> Exp -> Exp
ifte p te fe = (p && te) || fe

-- class Raw e where
--   raw :: String -> e

-- instance rawExp :: Raw Exp where
--   raw = RawExp

-- instance rawStat :: Raw Stat where
--   raw = RawStat


fn0 :: Block Exp -> Exp
fn0 = Fun0

fn1 :: (Exp -> Block Exp) -> Exp
fn1 = Fun1

fn2 :: (Exp -> Exp -> Block Exp) -> Exp
fn2 = Fun2

fn3 :: (Exp -> Exp -> Exp -> Block Exp) -> Exp
fn3 = Fun3

fn4 :: (Exp -> Exp -> Exp -> Exp -> Block Exp) -> Exp
fn4 = Fun4

class Functionable f where
  fn :: f -> Exp

instance functionable0 :: Functionable (Block Exp) where
  fn = fn0

instance functionable1 :: Functionable (Exp -> Block Exp) where
  fn = fn1

instance functionable2 :: Functionable (Exp -> Exp -> Block Exp) where
  fn = fn2

instance functionable3 :: Functionable (Exp -> Exp -> Exp -> Block Exp) where
  fn = fn3

instance functionable4 :: Functionable (Exp -> Exp -> Exp -> Exp -> Block Exp) where
  fn = fn4

class Lambdable f where
  lam :: f -> Exp

instance lambdable0 :: Lambdable Exp where
  lam = lam0

instance lambdable1 :: Lambdable (Exp -> Exp) where
  lam = lam1

instance lambdable2 :: Lambdable (Exp -> Exp -> Exp) where
  lam = lam2

instance lambdable3 :: Lambdable (Exp -> Exp -> Exp -> Exp) where
  lam = lam3

instance lambdable4 :: Lambdable (Exp -> Exp -> Exp -> Exp -> Exp) where
  lam = lam4

lam0 :: Exp -> Exp
lam0 f = fn0 $ pure f

lam1 :: (Exp -> Exp) -> Exp
lam1 f = fn1 $ f >>> pure

lam2 :: (Exp -> Exp -> Exp) -> Exp
lam2 f = fn2 $ \v1 v2 -> pure $ f v1 v2

lam3 :: (Exp -> Exp -> Exp -> Exp) -> Exp
lam3 f = fn3 $ \v1 v2 v3 -> pure $ f v1 v2 v3

lam4 :: (Exp -> Exp -> Exp -> Exp -> Exp) -> Exp
lam4 f = fn4 $ \v1 v2 v3 v4 -> pure $ f v1 v2 v3 v4

class Runable e where
  run :: Exp -> e

instance runable0 :: Runable Exp where
  run = run0

instance runable1 :: Runable (Exp -> Exp) where
  run = run1

instance runable2 :: Runable (Exp -> Exp -> Exp) where
  run = run2

instance runable3 :: Runable (Exp -> Exp -> Exp -> Exp) where
  run = run3

instance runable4 :: Runable (Exp -> Exp -> Exp -> Exp -> Exp) where
  run = run4

instance runable5 :: Runable (Exp -> Exp -> Exp -> Exp -> Exp -> Exp) where
  run = run5

instance runable6 :: Runable (Exp -> Exp -> Exp -> Exp -> Exp -> Exp -> Exp) where
  run = run6

instance runable7 :: Runable (Exp -> Exp -> Exp -> Exp -> Exp -> Exp -> Exp -> Exp) where
  run = run7

run0 :: Exp -> Exp
run0 f = FunCall f []

run1 :: Exp -> Exp -> Exp
run1 f x1 = FunCall f [x1]

run2 :: Exp -> Exp -> Exp -> Exp
run2 f x1 x2 = FunCall f [x1, x2]

run3 :: Exp -> Exp -> Exp -> Exp -> Exp
run3 f x1 x2 x3 = FunCall f [x1, x2, x3]

run4 :: Exp -> Exp -> Exp -> Exp -> Exp -> Exp
run4 f x1 x2 x3 x4 = FunCall f [x1, x2, x3, x4]

run5 :: Exp -> Exp -> Exp -> Exp -> Exp -> Exp -> Exp
run5 f x1 x2 x3 x4 x5 = FunCall f [x1, x2, x3, x4, x5]

run6 :: Exp -> Exp -> Exp -> Exp -> Exp -> Exp -> Exp -> Exp
run6 f x1 x2 x3 x4 x5 x6 = FunCall f [x1, x2, x3, x4, x5, x6]

run7 :: Exp -> Exp -> Exp -> Exp -> Exp -> Exp -> Exp -> Exp -> Exp
run7 f x1 x2 x3 x4 x5 x6 x7 = FunCall f [x1, x2, x3, x4, x5, x6, x7]

class LitLua e where
  lit :: e -> Exp

instance litLuaInt :: LitLua Int where
  lit = NumberInt

instance litLuaNumber :: LitLua Number where
  lit = Number

instance litLuaString :: LitLua String where
  lit = String
  
instance litLuaBool :: LitLua Boolean where
  lit b = Bool b

instance litLuaTable :: (Homogeneous r Exp) => LitLua (Record r) where
  lit = table

instance litLuaList :: LitLua (Array Exp) where
  lit = list

rawString :: String -> Exp
rawString = RawString

rawFile :: String -> Exp
rawFile = RawFile
