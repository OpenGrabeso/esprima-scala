package esprima.port

import scala.language.implicitConversions

trait OrTypes {
  case class OrType(var value: Any = null) {
    def get[T]: T = value.asInstanceOf[T]
    def === [T](that: T): Boolean = get[T] == that
  }

  object OrType {
    implicit def orTypeAsNumber(src: OrType): Double = src.get[Int]
    implicit def orTypeAsString(src: OrType): String = src.get[String]

    implicit def orTypeFromNumber(src: Double): OrType = OrType(src)
    implicit def orTypeFromString(src: String): OrType = OrType(src)
  }


}
