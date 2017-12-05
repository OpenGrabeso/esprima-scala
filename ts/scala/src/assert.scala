/*
ScalaFromJS: 2017-12-05 14:33:13.940
assert.js
*/

package esprima
"use strict"
// Ensure the condition is true, otherwise throw an error.
// This is only to have a better contract semantic, i.e. another safety net
// to catch a logic error. The condition shall be fulfilled in normal case.
// Do NOT use this to enforce a certain condition on any user input.
Object.defineProperty(exports, "__esModule", new {
  var value = true
})

def assert(condition: Any, message: Any) = {
  /*istanbul ignore if */
  if (!condition) {
    throw new Error("ASSERT: " + message)
  }
}
exports.assert = assert
