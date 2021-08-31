/*
ScalaFromJS: Dev
error-handler.ts
*/

package com.github.opengrabeso.esprima

import scala.collection.mutable.ArrayBuffer

object ErrorHandler {
  class Error(message: String) extends Exception(message) {
    var name: String = _
    var index: Int = _
    var lineNumber: Int = _
    var column: Int = _
    var description: String = _
  }
}

import ErrorHandler._

/*tslint:disable:max-classes-per-file */

class ErrorHandler {
  var errors = ArrayBuffer.empty[Error]
  var tolerant: Boolean = false
  def recordError(error: Error): Unit = {
    this.errors.push(error)
  }
  
  def tolerate(error: Error): Unit = {
    if (this.tolerant) {
      this.recordError(error)
    } else {
      throw error
    }
  }
  
  def constructError(msg: String, column: Int): Error = {
    var error = new Error(msg)
    error.column = column
    error
  }
  
  def createError(index: Int, line: Int, col: Int, description: String): Error = {
    val msg = "Line " + line + ": " + description
    val error = this.constructError(msg, col)
    error.index = index
    error.lineNumber = line
    error.description = description
    error
  }
  
  def throwError(index: Int, line: Int, col: Int, description: String): Nothing = {
    throw this.createError(index, line, col, description)
  }
  
  def tolerateError(index: Int, line: Int, col: Int, description: String) = {
    val error = this.createError(index, line, col, description)
    if (this.tolerant) {
      this.recordError(error)
    } else {
      throw error
    }
  }
  
}
