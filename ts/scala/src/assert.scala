/*
ScalaFromJS: Dev 2020-02-24 19:42:52
assert.ts
*/

package esprima
def assert(condition: Boolean, message: String): Unit = {
  /*istanbul ignore if */
  if (!condition) {
    throw new Error("ASSERT: " + message)
  }
}