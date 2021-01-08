package com.github.opengrabeso.esprima
package port

import scala.collection.mutable.ArrayBuffer
import scala.language.implicitConversions
import scala.reflect.ClassTag

trait ArrayOps {
  implicit class ArrayBufferOps[T](a: ArrayBuffer[T]) {
    def push(x: T) = a append x
    def push(x: T, y: T, xx: T*) = {
      // form with a single element is used most often, we want it to be efficient
      // even if it means the mutliple elements form is a bit complicated and inefficient
      a append x
      a append y
      a appendAll xx
    }
    def unshift(x: T*) = a.insertAll(0, x)
    def shift(): T = {
      val r = a.head
      a.remove(0)
      r
    }
    def pop(): T = {
      a.remove(a.length - 1)
    }
    def concat(b: TraversableOnce[T]): ArrayBuffer[T] = {
      a.appendAll(b)
      a
    }

    def splice(start: Int, toDelete: Int, toInsert: T*): ArrayBuffer[T] = {
      a.remove(start, toDelete)
      a.insertAll(start, toInsert)
      a
    }

    def splice(start: Int): ArrayBuffer[T] = a.splice(start, a.length - start)

    def setLength: Int = a.length // getter not needed, but othewise we cannot defined the setter
    def setLength_= (l: Int) = {
      assert(l <= a.length) // verify we are shrinking only
      a.remove(l, a.size - l)
    }
  }

  implicit def bufferToArray[A, B >: A: ClassTag](a: ArrayBuffer[A]): Array[B] = {
    if (a != null) a.toArray[B]
    else null
  }


}
