"use strict"

const AST = require("luaparse").ast

const parse = require("luaparse").parse

const formatter = require("@appguru/luafmt").ast.formatter()

exports.parseString = parse

exports.chunk
  = AST.chunk

exports.assignmentStatementImpl
  = AST.assignmentStatement

exports.localStatementImpl
  = AST.localStatement

exports.returnStatement
  = AST.returnStatement

exports.literalString
  = function(s) { return AST.literal(2,null,s)}

exports.literalNumber
  = function(s) { return AST.literal(16,s,"0")}

exports.literalInt
  = function(s) { return AST.literal(16,s,"0")}

exports.literalBool
  = function(s) { return AST.literal(64,null,s ? "true" : "false")}

exports.literalNil
  = AST.literal(128,null,"nil")

exports.identifier
  = AST.identifier

exports.indexExpressionImpl
  = AST.indexExpression
exports.memberExpressionImpl
  = function(base, i) {return AST.memberExpression(base, '.', i)}

exports.functionStatementImpl = function(vs, b) {
  return AST.functionStatement(null, vs, false, b)
}

exports.callExpressionImpl = AST.callExpression

exports.tableConstructorExpression 
  = AST.tableConstructorExpression
exports.tableKeyStringImpl 
  = AST.tableKeyString
exports.tableValue
  = AST.tableValue

exports.binaryExpressionImpl 
  = AST.binaryExpression
exports.unaryExpressionImpl 
  = AST.unaryExpression

exports.renderAST = formatter

  // var EOF = 1, StringLiteral = 2, Keyword = 4, Identifier = 8
  //   , NumericLiteral = 16, Punctuator = 32, BooleanLiteral = 64
  //   , NilLiteral = 128, VarargLiteral = 256;
