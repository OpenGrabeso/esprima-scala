package com.github.opengrabeso.esprima
package port

import scala.collection.mutable

trait MapOps {
  def assign[T](target: mutable.Map[String, T], a: mutable.Map[String, T], b: mutable.Map[String, T]): Unit = {
    target.clear()
    target ++= a
    target ++= b
  }

  def has[T](map: mutable.Map[String, T], key: String): Boolean = map.contains(key)
}
