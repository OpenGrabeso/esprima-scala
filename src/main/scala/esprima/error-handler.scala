/*
ScalaFromJS: 2017-12-05 14:33:13.940
error-handler.js
*/

package esprima

import scala.collection.mutable.ArrayBuffer

object ErrorHandler {
  class Error(msg: String) extends Exception(msg) {
    var index: Int = _
    var lineNumber: Int = _
    var column: Int = _
    var description: String = _
  }
}

import ErrorHandler._

class ErrorHandler() {
  var errors = ArrayBuffer.empty[Error]
  var tolerant: Boolean = false
  def recordError(error: Error) = {
    this.errors.push(error)
  }
  
  def tolerate(error: Error) = {
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
  
  def createError(index: Int, line: Int, col: Int, description: String) = {
    val msg = "Line " + line + ": " + description
    val error = this.constructError(msg, col)
    error.index = index
    error.lineNumber = line
    error.description = description
    error
  }
  
  def throwError(index: Int, line: Int, col: Int, description: String) = {
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
