package com.github.opengrabeso.esprima

object Resources {
  private def rscPath(path: String): String = "jvm/src/test/resources/" + path
  def fromResource(str: String): String = {

    import scalajs.js.Dynamic.{global => g}
    val fs = g.require("fs")

    def readFile(name: String): String = fs.readFileSync(name).toString

    readFile(rscPath(str))
  }
}