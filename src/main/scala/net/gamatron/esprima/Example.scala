package net.gamatron.esprima

import scala.reflect.runtime.universe._
import esprima.Esprima.parse
import esprima.Node
import esprima.Node._

object Example extends App {
  val ast = parse("answer = 42")
  /*
  createWalkerForNode(typeTag[AssignmentExpression])
  createWalkerForNode(typeTag[FunctionDeclaration])
  createWalkerForNode(typeTag[SwitchStatement])
  */
}
