# purescript-tsuki
An opinioned EDSL to Lua

## WIPS

Opinion: functional & simple

EDSL: Embeded Domain Specific Language makes it possible to abstract patterns in purescript

## Get Start

```
npm install @appguru/luafmt
npm install luaparse
```
clone this repo to ``../purescript-tsuki``

add path to your ``packages.dhall``

```
...
in  upstream
 with tsuki = ../purescript-tsuki/spago.dhall as Location 
```
and ``spago.dhall``
```
dependencies = [ ... , "tsuki"]
```

## How to use

Basic AST:
```
Block a  -- lua chunk/block/statements
~ Writer [Stat]

Stat -- lua statements 

Exp -- lua expression
```
```
literal Value: 
  lit : e -> Exp
run function: 
  run : Exp -> Exp -> Exp
build function: 
  fn :  (Exp -> Block Exp) -> Exp
build pure function: 
  lam : (Exp -> Exp) -> Exp
string literal: 
  rawString : String -> Exp
external string from file: 
  rawFile : FilePath -> Exp
```
For examples, see ``test/Main.purs``
