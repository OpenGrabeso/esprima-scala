/*
ScalaFromJS: 2017-12-05 14:33:13.940
error-handler.js
*/

package esprima
"use strict"
/*tslint:disable:max-classes-per-file */
Object.defineProperty(exports, "__esModule", new {
  var value = true
})

class ErrorHandler() {
  var errors = Array.empty[Error]
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
  
  def constructError(msg: String, column: Any) = {
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
  
  def createError(index: Any, line: Any, col: Any, description: Any) = {
    val msg = "Line " + line + ": " + description
    val error = this.constructError(msg, col)
    error.index = index
    error.lineNumber = line
    error.description = description
    error
  }
  
  def throwError(index: Any, line: Any, col: Any, description: Any) = {
    throw this.createError(index, line, col, description)
  }
  
  def tolerateError(index: Any, line: Any, col: Any, description: Any) = {
    val error = this.createError(index, line, col, description)
    if (this.tolerant) {
      this.recordError(error)
    } else {
      throw error
    }
  }
  
}

exports.ErrorHandler = ErrorHandler
