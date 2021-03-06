package com.github.opengrabeso.esprima.port

import scala.language.implicitConversions
import scala.reflect.ClassTag

trait OrTypes {
  case class OrType(var value: Any = null) {
    override def toString = if (value != null) value.toString else "null"
    def get[T]: T = value.asInstanceOf[T]
    def is[T: ClassTag]: Boolean = value match {
      case _: T => true
      case _ => false
    }
    def === [T](that: T): Boolean = get[T] == that
    def !== [T](that: T): Boolean = get[T] != that
  }

  object OrType {
    implicit def orTypeAsNumber(src: OrType): Double = src.get[Int]
    implicit def orTypeAsString(src: OrType): String = src.get[String]

    implicit def orTypeFromInt(src: Int): OrType = OrType(src.toDouble)
    implicit def orTypeFromNumber(src: Double): OrType = OrType(src)
    implicit def orTypeFromString(src: String): OrType = OrType(src)
    implicit def orTypeFromBoolean(src: Boolean): OrType = OrType(src)
  }


}
