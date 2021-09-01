package com.github.opengrabeso.esprima

sealed trait OutputType {
  def output(program: Node.Program): String
}
object OutputType {
  object AST extends OutputType {
    import walker._
    walker.allWalkers ++= walker.createWalkers[Node.Node, Node.type]
    var level = 0
    def output(program: Node.Program) = {
      val sb = new StringBuilder
      walker.walkRecursive(program) { node =>
        sb ++= "  " * level + node.getClass.getSimpleName
        sb ++= "\n"
        level += 1
        false
      }{_ => level -= 1}
      sb.result()
    }
  }
  object ToString extends OutputType {
    def output(program: Node.Program) = program.toString
  }
}

