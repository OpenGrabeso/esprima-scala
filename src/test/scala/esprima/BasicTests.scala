package esprima

import org.scalatest.FunSuite

import Esprima._

class BasicTests extends FunSuite {
  val input ="answer = 42"
  //language=JavaScript
  val es6 = """
      class Node {
        type () {return "Node"}
      }

      class Identifier extends Node {
        type () {return Syntax.Identifier}
      }

      class Literal extends Node {
        type () {return Syntax.Literal}
      }

      Syntax = {
        Identifier : 'Identifier',
        Literal: 'Literal'
      };

      function useIfSimple(key) {
        if (key.type === Syntax.Identifier) {
          return key.name
        }
        return ""
      }

      function useExpr(key) {
        return key.type === Syntax.Identifier && key.name === value
      }

      function useIf(key) {
        if (key.type === Syntax.Identifier && key.name.lenght > 0) {
          return key.name
        }
        return ""
      }

      function useExprComplex(key, value) {
      }

      var ScalaFromJS_settings = {
        members: [
          {
            cls: ".*",
            name: "type",
            operation: "getClass"
          }
        ]
      }
      """

  test("Tokenizer - simple input") {
    val tokens = tokenize(input)
    println(tokens)
  }

  test("Parser - simple input") {
    val tree = parse(input)
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

}
