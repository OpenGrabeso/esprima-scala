package com.github.opengrabeso.esprima

object Resources {
  def fromResource(str: String): String = {
    val absPath = if (str.startsWith("/")) str else "/" + str
    scala.io.Source.fromInputStream(getClass.getResourceAsStream(absPath)).mkString
  }
}