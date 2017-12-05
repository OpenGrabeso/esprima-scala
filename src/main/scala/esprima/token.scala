/*
ScalaFromJS: 2017-12-05 14:38:17.341
token.js
*/

package esprima

object TokenName {
  val a = Array(
    "Boolean",
    "<end>",
    "Identifier",
    "Keyword",
    "Null",
    "Numeric",
    "Punctuator",
    "String",
    "RegularExpression",
    "Template"
  )

  def apply(i: Int) = a.apply(i)
}

