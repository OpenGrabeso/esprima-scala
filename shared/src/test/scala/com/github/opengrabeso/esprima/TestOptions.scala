package com.github.opengrabeso.esprima

trait TestOptions {
  object ModOptions extends Parser.Options {
    range = true
    attachComment = true
    tolerant = true
    typescript = true
    sourceType = "module" // allow exports
  }
}
