package esprima
package port

import scala.collection.mutable.ArrayBuffer
import scala.language.implicitConversions
import scala.reflect.ClassTag

trait ArrayOps {
  implicit class ArrayBufferOps[T](a: ArrayBuffer[T]) {
    def push(x: T) = a append x
    def pop(): T = {
      val ret = a.last
      a.dropRight(1)
      ret
    }
    def concat(b: TraversableOnce[T]): a.type = {
      a.appendAll(b)
      a
    }
  }

  implicit def bufferToArray[A, B >: A: ClassTag](a: ArrayBuffer[A]): Array[B] = a.toArray[B]


}
