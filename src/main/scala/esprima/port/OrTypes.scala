package esprima.port

import scala.language.implicitConversions

trait OrTypes {
  case class OrType(var value: Any = null)

  object OrType {
    implicit def orTypeAsInt(src: OrType): Int = src.value.asInstanceOf[Int]
    implicit def orTypeAsString(src: OrType): String = src.value.asInstanceOf[String]

    implicit def orTypeFromInt(src: Int): OrType = OrType(src)
    implicit def orTypeFromString(src: String): OrType = OrType(src)
  }


}
