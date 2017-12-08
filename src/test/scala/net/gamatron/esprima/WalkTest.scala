package net.gamatron.esprima

import esprima.Esprima._
import esprima.Node
import esprima.Node._
import org.scalatest.FunSuite

class WalkTest extends FunSuite with TestInputs {

  test("Walk simple expression") {
    val ast = parse(answer42)
    var hit = false
    walk(ast) {
      case node: AssignmentExpression =>
        hit = true
        false
      case node =>
        false
    }
    assert(hit)
  }

  test("Walk complex expression") {
    val ast = parse(es6)
    var countBinary = 0
    var countOther = 0
    walk(ast) {
      case _: BinaryExpression =>
        countBinary += 1
        false
      case _ =>
        countOther += 1
        false
    }
    assert(countBinary >= 7)
    assert(countOther >= 100)
  }

  test("Walk three.js") {
    val ast = parse(threeSource)
    var countFunctions = 0
    walk(ast) {
      case node: FunctionExpression =>
        countFunctions += 1
        false
      case node =>
        false
    }
    assert(countFunctions >= 1000)
  }

  test("transformBefore") {

  }

  test("transformAfter") {

  }


}
