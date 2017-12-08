package net.gamatron

import _root_.esprima.Node.Node

package object esprima {
  // interface inspired by uglify-js
  def walk(ast: Node)(callback: Node => Boolean) = {
    ???
  }

  def transformBefore() = ???

  def transformAfter() = ???


}
