package net.gamatron.esprima
package symbols

import org.scalatest.FunSuite
import esprima.Esprima._

class SymbolsTest extends FunSuite with TestInputs {

  test("Walk with scope tracking ") {
    val ast = parse(es6)
    var maxScopeDepth = 0
    var maxNodeDepth = 0
    walk(ast) { (node, context) =>
      maxScopeDepth = maxScopeDepth max context.scopes.length
      maxNodeDepth = maxNodeDepth max context.parents.length
      false
    }
    println(s"Max scope depth $maxScopeDepth")
    println(s"Max node depth $maxNodeDepth")
    assert(maxScopeDepth > 0)
    assert(maxNodeDepth > 0)
  }

  test("Walk three.js with scope tracking ") {
    val ast = parse(threeSource)
    var maxScopeDepth = 0
    var maxNodeDepth = 0
    walk(ast) { (node, context) =>
      maxScopeDepth = maxScopeDepth max context.scopes.length
      maxNodeDepth = maxNodeDepth max context.parents.length
      false
    }
    println(s"Max scope depth $maxScopeDepth")
    println(s"Max node depth $maxNodeDepth")
    assert(maxScopeDepth > 0)
    assert(maxNodeDepth > 0)
  }

  test("List all symbols") {
    val ast = parse(es6)
    val syms = listAllSymbols(ast)
    println(syms)
  }
}
