module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)

import Tsuki
import Tsuki.AST
import Tsuki.Render

import Data.HeytingAlgebra

main :: Effect Unit
main = do
  -- renderToFile prizeDraw "test.lua"
  r <- render prizeDraw
  log r
  log "done"

funLib = "lib/fun.lua"
lumeLib = "lib/lume.lua"

loadFile :: String -> Exp
loadFile path = run $ run1 (ident "load") $ rawFile path

whichAreIn :: Block Exp
whichAreIn = do
  fun <- newVar $ loadFile funLib
  lume <- newVar $ loadFile lumeLib
  solution <- newVar $ lit {}
  let totable = run1 $ get fun "totable"
  let filter = run2 $ get fun "filter"
  let any = run2 $ get fun "any"
  let sort = run1 $ get lume "sort"
  let unique = run1 $ get lume "unique"
  -- let stringfind = run2 $ get (ident "string") "find"
  let stringfindPlain x1 x2 = run4 (get (ident "string") "find") x1 x2 (lit 1) tt
  set solution "inArray" $ lam $ \arr1 arr2 ->
     sort $ unique $ totable $ filter (lam $ \arr1x -> any (lam (\arr2x -> not $ equals nil $ stringfindPlain arr2x arr1x)) arr2) arr1
  pure solution
  

prizeDraw :: Block Exp
prizeDraw = do
  lume <- newVar $ loadFile lumeLib
  fun <- newVar $ loadFile funLib
  solution <- newVar $ lit {}
  set solution "rank" $ lam3 $ prizeDraw_ lume fun
  pure solution

prizeDraw_ :: Exp -> Exp -> Exp -> Exp -> Exp -> Exp
prizeDraw_ lume fun sts' wes rankN = ifte (equals m $ lit 0) (lit "No participants") (ifte (gt rankN m) (lit "Not enough participants") rr)
  where
    range = toArray $ run2 (get fun "range") (lit 1) m
    sts = toArray $ seperateComma sts'
    stwes = lmap range $ lam \i -> lit {name: (gett sts i), weight: (gett wes i)}
    seperateComma str = run2 (get (ident "string") "gmatch") str $ lit "([^,]+)"
    toArray = run1 $ get lume "array"
    lmap = run2 $ get lume "map"
    -- lfind = run $ get lume "find"
    fromString str = run2 (get (ident "string") "gmatch") str $ lit "%w"
    getCode c = run1 (get (ident "string") "byte") c - lit 96
    reduce = run2 (get lume "reduce")
    lower = run1 (get (ident "string") "lower")
    sortBy = run2 (get lume "sort")
    m = len sts
    sortFn = lam $ \l r -> (gt (get l "rank") (get r "rank")) || ifte (equals (get l "rank") (get r "rank")) (gt (get r "name") (get l "name")) ff
    res = lmap stwes $ lam $ \stwe -> lit { name: get stwe "name", rank: get stwe "weight" * (reduce (lmap (toArray $ fromString $ get stwe "name") (lam $ lower >>> getCode)) (lam2 $ add) + len (get stwe "name"))}
    rr = get (gett (sortBy res sortFn) rankN) "name"
