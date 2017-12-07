package esprima

import org.scalatest.FunSuite

import Esprima._

class BasicTests extends FunSuite {
  val input ="answer = 42"
  test("Tokenizer") {
    tokenize(input, new Parser.Options, null)
  }

  test("Parser") {
    parse(input, new Parser.Options, null)
  }

}
