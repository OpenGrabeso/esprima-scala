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
          Some[TermCallback]{(oMirror, callback) =>
            oMirror.reflectField(term).get.asInstanceOf[Seq[Node]].foreach(callback)
          }
        case NullaryMethodType(resultType) if resultType <:< typeOf[Array[_]] =>
          resultType.typeArgs match {
            case at :: Nil if at <:< typeOf[Node] =>
              Some[TermCallback]{(oMirror, callback) =>
                oMirror.reflectField(term).get.asInstanceOf[Array[Node]].foreach(callback)
              }
            case _ =>
              None
          }
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
