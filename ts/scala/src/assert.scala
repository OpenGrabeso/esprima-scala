/*
ScalaFromJS: Dev
assert.ts
*/

package com.github.opengrabeso.esprima
def assert(condition: Boolean, message: String): Unit = {
  /*istanbul ignore if */
  if (!condition) {
    throw new Error("ASSERT: " + message)
  }
}