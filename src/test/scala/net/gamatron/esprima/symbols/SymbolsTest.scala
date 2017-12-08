package net.gamatron.esprima
package symbols

import org.scalatest.FunSuite
import esprima.Esprima._

class SymbolsTest extends FunSuite with TestInputs {

  test("Walk with scope tracking ") {
    val ast = parse(es6)
    var maxDepth = 0
    walk(ast) { (node, context) =>
      maxDepth = maxDepth max context.scopes.length
      false
    }
    println(s"Max depth $maxDepth")
    assert(maxDepth > 0)
  }

  test("Walk three.js with scope tracking ") {
    val ast = parse(threeSource)
    var maxDepth = 0
    walk(ast) { (node, context) =>
      maxDepth = maxDepth max context.scopes.length
      false
    }
    println(s"Max depth $maxDepth")
    assert(maxDepth > 0)
  }

}
