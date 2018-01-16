/*
ScalaFromJS: Dev 2018-01-16 17:57:51
assert.js
*/

package esprima
def assert(condition: Boolean, message: String) = {
  /*istanbul ignore if */
  if (!condition) {
    throw new Error("ASSERT: " + message)
  }
}