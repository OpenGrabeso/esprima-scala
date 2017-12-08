package net.gamatron.esprima

import _root_.esprima.Node
import Node._

import scala.collection.mutable.ArrayBuffer

package object symbols {
  // create symbol lists for all relevant scopes


  /**
  * Walk while tracking a scope stack
  * */
  class ScopeContext {
    val scopes =  ArrayBuffer.empty[Node.Node]

  }

  def walk(node: Node, context: ScopeContext = new ScopeContext)(callback: (Node, ScopeContext) => Boolean): Unit = {

    if (node != null && !callback(node, context)) {
      val isScope = node.isInstanceOf[IsScope]
      if (isScope) {
        context.scopes.push(node)
      }

      walker.walkInto(node)(node => walk(node, context)(callback))

      if (isScope) {
        context.scopes.pop()
      }

    }
  }

}
