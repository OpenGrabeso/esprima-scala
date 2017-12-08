package net.gamatron.esprima

import _root_.esprima.Node
import Node._

import scala.collection.mutable.ArrayBuffer

package object symbols {
  // create symbol lists for all relevant scopes
  class ScopeInfo {
    var symbols = Set.empty[String] // all symbols defined in the scope
  }

  class ScopeContext {
    val parents = ArrayBuffer.empty[Node.Node]
    val scopes =  ArrayBuffer.empty[(Node.Node, ScopeInfo)]

  }

  /**
    * Walk while tracking a scope stack and a symbol information
    * */
  def walk(node: Node, context: ScopeContext = new ScopeContext)(callback: (Node, ScopeContext) => Boolean): Unit = {

    if (node != null && !callback(node, context)) {
      val isScope = node.isInstanceOf[IsScope]
      if (isScope) {
        context.scopes.push(node -> new ScopeInfo)
      }
      context.parents.push(node) // TODO: optimize: no push when walkInto is empty

      node match {
        case Node.VariableDeclarator(Node.Identifier(id), _) =>
          context.scopes.last._2.symbols += id
        case _ =>
      }

      walker.walkInto(node)(node => walk(node, context)(callback))

      context.parents.pop()
      if (isScope) {
        context.scopes.pop()
      }

    }
  }

}
