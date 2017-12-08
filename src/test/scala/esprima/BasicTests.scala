package esprima

import org.scalatest.FunSuite
import Esprima._
import net.gamatron.esprima.TestInputs

class BasicTests extends FunSuite with TestInputs {
  //language=JavaScript

  test("Tokenizer - simple input") {
    val tokens = tokenize(answer42)
    println(tokens)
  }

  test("Parser - simple input") {
    val tree = parse(answer42)
    println(tree)
  }

  test("Tokenizer - more complex es6 input") {
    val tokens = tokenize(es6)
    println(tokens)
  }

  test("Parser - more complex es6 input") {
    val tree = parse(es6)
    println(tree)
  }

  test("Parse three.js") {
    val tree = parse(threeSource)
    println(tree)
  }
}
