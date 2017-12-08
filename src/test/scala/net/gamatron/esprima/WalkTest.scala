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
    var count = 0
    val countNodes = collection.mutable.Map.empty[Class[_], Int].withDefaultValue(0)
    walk(ast) { node =>
      count += 1
      countNodes(node.getClass) += 1
      false
    }
    // verify node counts are sensible (we are really walking the tree)
    println(s"Total: $count")
    println("  " + countNodes.toSeq.map { case (k, v) => k.getSimpleName -> v }.sortBy(_._2).reverse.take(10).mkString("\n  "))
    assert(countNodes(classOf[Identifier]) >= 50000)
    assert(countNodes(classOf[StaticMemberExpression]) >= 10000)
    assert(countNodes(classOf[FunctionExpression]) >= 1000)
    assert(count >= 100000)
  }

  test("transformBefore") {

  }

  test("transformAfter") {

  }


}
