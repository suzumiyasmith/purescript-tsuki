module Tsuki.Render 
  (  render
  , renderToFile) where

import Prelude

import Tsuki.AST

import Effect
import Effect.Class

import Node.FS.Sync
import Node.Encoding

import Data.Tuple
import Data.Traversable
-- import Data.Monoid
import Data.Function.Uncurried
import Control.Monad.Writer
import Control.Monad.State

foreign import data AST :: Type
foreign import chunk :: Array AST -> AST

foreign import assignmentStatementImpl :: Fn2 (Array AST) (Array AST) AST
assignmentStatement :: Array AST -> Array AST -> AST
assignmentStatement = runFn2 assignmentStatementImpl
foreign import localStatementImpl :: Fn2 (Array AST) (Array AST) AST
localStatement :: Array AST -> Array AST -> AST
localStatement = runFn2 localStatementImpl
foreign import returnStatement :: Array AST -> AST

foreign import literalString :: String -> AST
foreign import literalNumber :: Number -> AST
foreign import literalInt :: Int -> AST
foreign import literalBool :: Boolean -> AST
foreign import literalNil :: AST

foreign import identifier :: Name -> AST

foreign import indexExpressionImpl :: Fn2 AST AST AST
indexExpression :: AST -> AST -> AST
indexExpression = runFn2 indexExpressionImpl
foreign import memberExpressionImpl :: Fn2 AST AST AST
memberExpression :: AST -> AST -> AST
memberExpression = runFn2 memberExpressionImpl

foreign import functionStatementImpl :: Fn2 (Array AST) (Array AST) AST
functionStatement :: Array AST -> Array AST -> AST
functionStatement = runFn2 functionStatementImpl

foreign import callExpressionImpl :: Fn2 AST (Array AST) AST
callExpression :: AST -> Array AST -> AST
callExpression = runFn2 callExpressionImpl

foreign import tableConstructorExpression :: Array AST -> AST
foreign import tableValue :: AST -> AST
foreign import tableKeyStringImpl :: Fn2 AST AST AST
tableKeyString :: AST -> AST -> AST
tableKeyString = runFn2 tableKeyStringImpl

foreign import binaryExpressionImpl :: Fn3 Name AST AST AST
binaryExpression :: Name -> AST -> AST -> AST
binaryExpression = runFn3 binaryExpressionImpl

foreign import unaryExpressionImpl :: Fn2 Name AST AST
unaryExpression :: Name -> AST -> AST
unaryExpression = runFn2 unaryExpressionImpl

foreign import parseString :: String -> AST

foreign import renderAST :: AST -> String


type Fresh = {freshVar :: Int, freshDepth :: Int}
type RenderM = StateT Fresh Effect

renderToFile :: Block Exp -> String -> Effect Unit
renderToFile b path = render b >>= writeTextFile UTF8 path

render :: Block Exp -> Effect String
render b = renderAST <$> renderM b

renderM :: Block Exp -> Effect AST
renderM b = evalStateT (renderBlock b) {freshVar: 0, freshDepth: 0}

renderBlock :: Block Exp -> RenderM AST
renderBlock b = do
  renderBlock' b $ \sts a -> do
    rs <- traverse renderStat sts
    r <- renderExp a
    pure $ chunk $ rs <> [returnStatement [r]]

-- renderBlock :: Block Exp -> AST
-- render b = let Tuple a sts = runWriter b in chunk $ (renderStat <$> sts) <> [returnStatement [renderExp a]]

renderBlock' :: forall a1. Block Exp -> (Array Stat -> Exp -> RenderM a1) -> RenderM a1
renderBlock' b f = do
  i <- getDepth
  let Tuple (Tuple a j) sts = runWriter $ runStateT b i
  withDepth j do
    f sts a

renderStat :: Stat -> RenderM AST
renderStat (Assign e1 e2) = do
  r1 <- renderExp e1
  r2 <- renderExp e2
  pure $ assignmentStatement [r1] [r2]
renderStat (LocalAssign e1 e2) = do
  r1 <- renderExp e1
  r2 <- renderExp e2
  pure $ localStatement [r1] [r2]
-- renderStat (RawStat a) = pure $ parseString a

renderExp :: Exp -> RenderM AST
renderExp (Bool b) = pure $ literalBool b
renderExp (String s) = pure $ literalString $ show s
renderExp (RawString s) = pure $ literalString $ "[====[" <> s <> "]====]"
renderExp (RawFile path) = do
  s <- liftEffect $ readTextFile UTF8 path
  renderExp $ RawString s
renderExp (Number s) = pure $ literalNumber s
renderExp (NumberInt s) = pure $ literalInt s
renderExp (Ident n) = pure $ identifier n
renderExp Nil = pure $ literalNil
renderExp (Index t (String k)) = do
  rt <- (renderExp t)
  rk <- renderExp (Ident k)
  pure $ memberExpression rt rk
renderExp (Index t k) = do
  rt <- renderExp t
  rk <- renderExp  k
  pure $ indexExpression rt rk

renderExp (Fun0 b) = do
  renderBlock' b $ \sts a -> do
    rs <- traverse renderStat sts
    r <- renderExp a
    pure $ functionStatement [] $ rs <> [returnStatement [r]]

renderExp (Fun1 b) = do
  v1 <- getVar
  renderBlock' (b v1) $ \sts a -> do
    rs <- traverse renderStat sts
    vs <- traverse renderExp [v1]
    r <- renderExp a
    endFun 1
    pure $ functionStatement vs $ rs <> [returnStatement [r]]

renderExp (Fun2 b) = do
  v1 <- getVar
  v2 <- getVar
  renderBlock' (b v1 v2) $ \sts a -> do
    rs <- traverse renderStat sts
    vs <- traverse renderExp [v1, v2]
    r <- renderExp a
    endFun 2
    pure $ functionStatement vs $ rs <> [returnStatement [r]]

renderExp (Fun3 b) = do
  v1 <- getVar
  v2 <- getVar
  v3 <- getVar
  renderBlock' (b v1 v2 v3) $ \sts a -> do
    rs <- traverse renderStat sts
    vs <- traverse renderExp [v1, v2, v3]
    r <- renderExp a
    endFun 3
    pure $ functionStatement vs $ rs <> [returnStatement [r]]

renderExp (Fun4 b) = do
  v1 <- getVar
  v2 <- getVar
  v3 <- getVar
  v4 <- getVar
  renderBlock' (b v1 v2 v3 v4) $ \sts a -> do
    rs <- traverse renderStat sts
    vs <- traverse renderExp [v1, v2, v3, v4]
    r <- renderExp a
    endFun 4
    pure $ functionStatement vs $ rs <> [returnStatement [r]]

renderExp (FunCall f xs) = do
  r1 <- renderExp f
  rs <- traverse renderExp xs
  pure $ callExpression r1 rs

renderExp (TableConst vs) =
  tableConstructorExpression <$> traverse (\(Tuple k v) -> tableKeyString <$> renderExp (Ident k) <*> renderExp v) vs
renderExp (ListConst vs) = tableConstructorExpression <$> traverse (renderExp >>> map tableValue) vs

renderExp (Binop n e1 e2) =
  binaryExpression n <$> renderExp e1 <*> renderExp e2
renderExp (Unop n e) =
  unaryExpression n <$> renderExp e

-- renderExp (RawExp a) = pure $ parseString a

getVar :: RenderM Exp
getVar = do
  v <- gets (\fresh -> Ident $ "var_" <> show fresh.freshVar)
  modify_ (\k -> {freshVar: k.freshVar + 1, freshDepth: k.freshDepth})
  pure v

endFun :: Int -> RenderM Unit
endFun i =
  modify_ (\k -> {freshVar: k.freshVar - i, freshDepth: k.freshDepth})

getDepth :: RenderM Int
getDepth = do
  i <- gets (\fresh -> fresh.freshDepth)
  pure i


withDepth :: forall a. Int -> RenderM a -> RenderM a
withDepth j b = do
  modify_ (\k -> {freshVar: k.freshVar, freshDepth: k.freshDepth + j})
  r <- b
  modify_ (\k -> {freshVar: k.freshVar, freshDepth: k.freshDepth - j})
  pure r

-- toPara :: Int -> RenderM (Array Exp)
-- toPara 0 = pure []
-- toPara i = do
--   fresh <- get
--   pure $ (\k -> Ident $ "var_" <> show (k + fresh)) <$> [1..i]

-- toPara 0 = []
