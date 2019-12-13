/*
ScalaFromJS: 2017-12-05 14:38:17.341
token.js
*/

package com.github.opengrabeso.esprima

object TokenName {
  val a = Array(
    "Undefined",
    "Boolean",
    "<end>",
    "Identifier",
    "Keyword",
    "Null",
    "Numeric",
    "Punctuator",
    "String",
    "RegularExpression",
    "Template",
    "TypeAnnotation"
  )

  def apply(i: Token.Token) = a.apply(i.id)
}

object Token extends Enumeration {
  type Token = Value

  val Undefined, // 0

  BooleanLiteral, // 1
  EOF, // 2
  Identifier, // 3
  Keyword, // 4
  NullLiteral, // 5
  NumericLiteral, // 6
  Punctuator, // 7
  StringLiteral, // 8
  RegularExpression, // 9
  Template, // 10
  TypeAnnotation // 11 - TypeScript
  = Value
}