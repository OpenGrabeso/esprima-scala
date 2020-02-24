/*
ScalaFromJS: Dev 2020-02-24 19:42:52
error-handler.ts
*/

package esprima
/*tslint:disable:max-classes-per-file */

class Error() {
  var name: String = _
  var message: String = _
  var index: Double = _
  var lineNumber: Double = _
  var column: Double = _
  var description: String = _

  def constructor(message: String)
  
}

class ErrorHandler() {
  var errors = Array.empty[Error]
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
  
  def constructError(msg: String, column: Double): Error = {
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
  
  def createError(index: Double, line: Double, col: Double, description: String): Error = {
    val msg = "Line " + line + ": " + description
    val error = this.constructError(msg, col)
    error.index = index
    error.lineNumber = line
    error.description = description
    error
  }
  
  def throwError(index: Double, line: Double, col: Double, description: String): never = {
    throw this.createError(index, line, col, description)
  }
  
  def tolerateError(index: Double, line: Double, col: Double, description: String) = {
    val error = this.createError(index, line, col, description)
    if (this.tolerant) {
      this.recordError(error)
    } else {
      throw error
    }
  }
  
}

