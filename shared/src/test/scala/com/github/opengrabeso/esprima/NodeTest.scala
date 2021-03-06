package com.github.opengrabeso.esprima

import org.scalatest.funsuite.AnyFunSuite

class NodeTest extends AnyFunSuite {
  test("Nodes can clone themselves") {
    val a = Node.AssignmentExpression("=", Node.Identifier("answer"), Node.Literal(42, "42"))
    val cloned = a.clone()
    assert(a.getClass == cloned.getClass)
    assert(a == cloned)
  }

  test("Nodes copy base properties on cloning themselves") {
    val a = Node.AssignmentExpression("=", Node.Identifier("answer"), Node.Literal(42, "42"))
    a.range = (0, 1)
    val cloned = a.clone()
    assert(a.range == cloned.range)


  }
}
