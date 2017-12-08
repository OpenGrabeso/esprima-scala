package net.gamatron.esprima

import esprima.Esprima._
import esprima.Node
import esprima.Node.AssignmentExpression
import org.scalatest.FunSuite

class WalkTest extends FunSuite with TestInputs {

  test("walk") {
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
  test("transformBefore") {

  }

  test("transformAfter") {

  }


}
