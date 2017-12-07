package esprima

import org.scalatest.FunSuite

import Esprima._

class BasicTests extends FunSuite {
  val input ="answer = 42"
  test("Tokenizer") {
    val tokens = tokenize(input)
    println(tokens)
  }

  test("Parser") {
    val tree = parse(input)
    println(tree)
  }

}
