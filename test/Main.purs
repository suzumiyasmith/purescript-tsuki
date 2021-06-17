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
  let totable = run1 $ get "totable" fun
  let filter = run2 $ get "filter" fun
  let any = run2 $ get "any" fun
  let sort = run1 $ get "sort" lume
  let unique = run1 $ get "unique" lume
  -- let stringfind = run2 $ get (ident "string") "find"
  let stringfindPlain x1 x2 = run4 (get "find" (ident "string")) x1 x2 (lit 1) tt
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
    range = toArray $ run2 (get "range" fun) (lit 1) m
    sts = toArray $ seperateComma sts'
    stwes = lmap range $ lam \i -> lit {name: (gett i sts), weight: (gett i wes)}
    seperateComma str = run2 (get "gmatch" (ident "string")) str $ lit "([^,]+)"
    toArray = run1 $ get "array" lume
    lmap = run2 $ get "map" lume
    -- lfind = run $ get lume "find"
    fromString str = run2 (get "gmatch" (ident "string")) str $ lit "%w"
    getCode c = run1 (get "byte" (ident "string")) c - lit 96
    reduce = run2 (get "reduce" lume)
    lower = run1 (get "lower" (ident "string"))
    sortBy = run2 (get "sort" lume)
    m = len sts
    sortFn = lam $ \l r -> (gt (get "rank" l) (get "rank" r)) || ifte (equals (get "rank" l) (get "rank" r)) (gt (get "name" r) (get "name" l)) ff
    res = lmap stwes $ lam $ \stwe -> lit { name: get "name" stwe, rank: get "weight" stwe * (reduce (lmap (toArray $ fromString $ get "name" stwe) (lam $ lower >>> getCode)) (lam2 $ add) + len (get "name" stwe))}
    rr = get "name" $ gett rankN (sortBy res sortFn) 
