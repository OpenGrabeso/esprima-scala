package esprima
package port

import scala.language.implicitConversions

trait BooleanConditions {

  implicit class OrValues[T <: AnyRef](a: T) {
    def || (b: T): T = if (a != null) a else b
  }

  implicit def toCondition[T <: AnyRef](a: T): Boolean = a != null

  implicit def toCondition(a: Int): Boolean = a != 0
}
