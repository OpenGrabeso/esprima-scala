/*
ScalaFromJS: Dev
token.ts
*/

package com.github.opengrabeso.esprima

import scala.collection.mutable

object Token extends Enumeration {
  type Token = Value

  val Undefined = Value
  val BooleanLiteral = Value(1)
  val EOF = Value
  val Identifier = Value
  val Keyword = Value
  val NullLiteral = Value
  val NumericLiteral = Value
  val Punctuator = Value
  val StringLiteral = Value
  val RegularExpression = Value
  val Template = Value
}

object TokenName {
  val TokenName = mutable.Map.empty[Token.Value, String]
  TokenName(Token.Undefined) = "Undefined"
  TokenName(Token.BooleanLiteral) = "Boolean"
  TokenName(Token.EOF) = "<end>"
  TokenName(Token.Identifier) = "Identifier"
  TokenName(Token.Keyword) = "Keyword"
  TokenName(Token.NullLiteral) = "Null"
  TokenName(Token.NumericLiteral) = "Numeric"
  TokenName(Token.Punctuator) = "Punctuator"
  TokenName(Token.StringLiteral) = "String"
  TokenName(Token.RegularExpression) = "RegularExpression"
  TokenName(Token.Template) = "Template"

  def apply(i: Token.Token) = TokenName(i)
}
