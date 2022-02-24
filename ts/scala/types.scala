/*
ScalaFromJS: Dev
types.ts
*/



/* import {SourceLocation, Comment} from "./src/scanner" */
import esprima.{SourceLocation,Comment}

trait Node {
  var `type`: (Unit) => Any = _
  var range = Array.empty[Double]
  var loc: SourceLocation = _
  var leadingComments = Array.empty[Comment]
  var innerComments = Array.empty[Comment]
  var trailingComments = Array.empty[Comment]
}

