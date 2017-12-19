package com.github.opengrabeso.esprima.port

import scala.reflect.runtime.currentMirror

object MapFromObject {
  def mapFromObject[T](m: Any): Map[String, T] = {

    val anyMirror = currentMirror.reflect(m)
    val items = for {
      symbol <- currentMirror.classSymbol(m.getClass).toType.members
      if symbol.isTerm && !symbol.isMethod && !symbol.isModule // && symbol.info.widen <:< baseType
    } yield {
      val field = anyMirror.reflectField(symbol.asTerm)
      // not sure why "trim" is needed, but some ShaderLib names contained spaces otherwise
      symbol.name.decodedName.toString.trim -> field.get.asInstanceOf[T]
    }
    items.toMap
  }

}
