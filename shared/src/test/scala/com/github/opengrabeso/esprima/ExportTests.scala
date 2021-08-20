package com.github.opengrabeso.esprima

import org.scalatest.flatspec.AnyFlatSpec

import Esprima._

class ExportTests extends AnyFlatSpec with TestInputs with TestOptions {

  behavior of "Parser"

  it should "parse exports" in {
    val ast = parse("""
     export * from './c';
    """, ModOptions)
    assert(ast.body.nonEmpty)
    assert(ast.errors.isEmpty)
  }

  it should "parse as exports" in {
    val ast = parse("""
     export * from './c';
     export { _M as M } from './m/M';
     export as namespace T;
     export * as M from './m/M';
    """, ModOptions)
    assert(ast.body.nonEmpty)
    assert(ast.errors.isEmpty)
  }
}
