package net.gamatron.esprima

import scala.reflect.runtime.universe._
import esprima.Esprima.parse
import esprima.Node
import esprima.Node._

object Example extends App {

  lazy val threeSource = scala.io.Source.fromResource("three.js").mkString

  val ast = parse(threeSource)

  {
    val time = System.currentTimeMillis()
    var count = 0
    val countNodes = collection.mutable.Map.empty[Class[_], Int].withDefaultValue(0)
    walk(ast) { node =>
      count += 1
      countNodes(node.getClass) += 1
      false
    }
    println(System.currentTimeMillis() - time)
    /*
  createWalkerForNode(typeTag[AssignmentExpression])
  createWalkerForNode(typeTag[FunctionDeclaration])
  createWalkerForNode(typeTag[SwitchStatement])
  */
    println(s"Total: $count")
    println(countNodes.toSeq.map { case (k, v) => k.getSimpleName -> v }.sortBy(_._2).reverse.take(10).mkString("\n"))
  }

  {
    val time = System.currentTimeMillis()
    val countNodes = collection.mutable.Map.empty[Class[_], Int].withDefaultValue(0)
    for (i <- 0 until 30) {
      walk(ast) {node =>
        countNodes(node.getClass) += 1
        false
      }
      println(System.currentTimeMillis() - time)
    }
  }
}
