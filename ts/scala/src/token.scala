/*
ScalaFromJS: Dev 2020-02-24 19:42:52
token.ts
*/

package esprima
object Token extends Enumeration {
  val BooleanLiteral = Value(1)
  val EOF = Value()
  val Identifier = Value()
  val Keyword = Value()
  val NullLiteral = Value()
  val NumericLiteral = Value()
  val Punctuator = Value()
  val StringLiteral = Value()
  val RegularExpression = Value()
  val Template = Value()
}

val TokenName = Map.empty[String, Unit]
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