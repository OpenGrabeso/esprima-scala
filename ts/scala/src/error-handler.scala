/*
ScalaFromJS: Dev 2018-01-16 17:57:51
error-handler.js
*/

package com.github.opengrabeso.esprima
class ErrorHandler() {
  var errors = Array.empty[Unit]
  var tolerant: Boolean = false
  def recordError(error: Any) = {
    this.errors.push(error)
  }
  
  def tolerate(error: Any) = {
    if (this.tolerant) {
      this.recordError(error)
    } else {
      throw error
    }
  }
  
  def constructError(msg: String, column: Double) = {
    var error = new Error(msg)
    try {
      throw error
    } catch {
      case base =>
        /*istanbul ignore else */
        if (Object.create && Object.defineProperty) {
          error = Object.create(base)
          Object.defineProperty(error, "column", new {
            var value = column
          })
        }
    }
    /*istanbul ignore next */
    error
  }
  
  def createError(index: Double, line: Double, col: Double, description: Any) = {
    val msg = "Line " + line + ": " + description
    val error = this.constructError(msg, col)
    error.index = index
    error.lineNumber = line
    error.description = description
    error
  }
  
  def throwError(index: Double, line: Double, col: Double, description: Any) = {
    throw this.createError(index, line, col, description)
  }
  
  def tolerateError(index: Double, line: Double, col: Double, description: Any) = {
    val error = this.createError(index, line, col, description)
    if (this.tolerant) {
      this.recordError(error)
    } else {
      throw error
    }
  }
  
}

