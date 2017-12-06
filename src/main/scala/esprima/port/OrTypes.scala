package esprima.port

import scala.language.implicitConversions

trait OrTypes {
  case class OrType(var value: Any = null) {
    def get[T] = value.asInstanceOf[T]
  }

  object OrType {
    implicit def orTypeAsInt(src: OrType): Int = src.get[Int]
    implicit def orTypeAsString(src: OrType): String = src.get[String]

    implicit def orTypeFromInt(src: Int): OrType = OrType(src)
    implicit def orTypeFromString(src: String): OrType = OrType(src)
  }


}
