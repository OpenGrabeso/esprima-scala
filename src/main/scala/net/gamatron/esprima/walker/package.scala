package net.gamatron.esprima

import _root_.esprima.Node.Node
import scala.reflect.runtime.universe._
import scala.reflect.runtime.currentMirror

package object walker {
  type TermCallback = (InstanceMirror, (Node) => Unit) => Unit
  type NodeWalker = Iterable[TermCallback]

  def createWalkerForNode[T <: Node](tag: TypeTag[T]): NodeWalker = {
    val members = typeOf[T](tag).members.filter(_.isTerm).map(_.asTerm).filter(_.isGetter)

    val walker: Iterable[TermCallback] = members.flatMap { term =>
      term.typeSignature match {
        case NullaryMethodType(resultType) if resultType <:< typeOf[Node] =>
          Some[TermCallback] {(oMirror, callback) =>
            callback(oMirror.reflectField(term).get.asInstanceOf[Node])
          }
        case NullaryMethodType(resultType) if resultType <:< typeOf[Seq[Node]] =>
          val tt = resultType
          println("Seq[Node]" + tt)
          None
        case NullaryMethodType(resultType) if resultType <:< typeOf[Array[Node]] =>
          val tt = resultType
          println("Array[Node] " + tt)
          None
        case NullaryMethodType(resultType) if resultType <:< typeOf[Seq[_]] =>
          val tt = resultType
          println("Seq[_] " + tt)
          println(tt)
          None
        case NullaryMethodType(resultType) if resultType <:< typeOf[Array[_]] =>
          resultType.typeArgs match {
            case at :: Nil if at <:< typeOf[Node] =>
              println("Array[+Node] " + at)
            case _ =>
          }
          val tt = resultType
          println(tt)
          println("Array[_] " + tt)
          None
        case t@NullaryMethodType(_) =>
          val tt = t
          //println(tt)
          None
        case  _ =>
          None
      }
    }

    walker
  }

  def walkNode(o: Node, walker: NodeWalker, callback: Node => Unit): Unit = {
    val oMirror = currentMirror.reflect(o)
    walker.foreach { w =>
      // walkNode(w)
      w(oMirror, callback)
    }
  }

}
