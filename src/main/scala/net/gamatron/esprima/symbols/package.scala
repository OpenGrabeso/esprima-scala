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
  def walk(node: Node)(callback: (Node, ScopeContext) => Boolean): Unit = {
    val context = new ScopeContext

    def callbackWithPrefix(node: Node): Boolean = {
      val ret = callback(node, context)
      if (!ret) {
        val isScope = node.isInstanceOf[IsScope]
        if (isScope) {
          context.scopes.push(node -> new ScopeInfo)
        }
        context.parents.push(node) // TODO: optimize: no push when walkInto is empty
      }
      ret
    }

    def post(node: Node) = {
      val isScope = node.isInstanceOf[IsScope]
      context.parents.pop()
      if (isScope) {
        context.scopes.pop()
      }
    }

    walker.walkRecursive(node)(callbackWithPrefix)(post)

    assert(context.scopes.isEmpty)
    assert(context.parents.isEmpty)
  }
}
