package com.github.opengrabeso.esprima

import Esprima._
import Node._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

// check if we can parse our own source code
class SelfParseTest extends AnyFlatSpec with TestInputs with Matchers {
  import java.io.File
  def processSubdirs(f: File, extension: String)(action: File => Unit): Unit = {
    Option(f.listFiles).foreach { list =>
      val (subdir, these) = list.partition(_.isDirectory)
      val tsOnly = these.filter(_.getName.endsWith(".ts")) // avoid processing JS or JSON files
      tsOnly.foreach(action)
      subdir.foreach(processSubdirs(_, extension)(action))
    }
  }

  def readFile(path: File): String = {
    val s = scala.io.Source.fromFile(path, "UTF-8")
    try {
      s.mkString
    } finally {
      s.close()
    }

  }
  object TSOptions extends Parser.Options {
    range = true
    attachComment = true
    tolerant = true
    typescript = true
    sourceType = "module" // allow exports
  }

  behavior of "Typescript parser"

  case class FileError(file: String, err: ErrorHandler.Error) extends Exception(file + ":" + err.getMessage)

  it should "Parse Esprima TS sources" in {
    val path = "ts/src/"
    processSubdirs(new File(path), ".ts") { f =>
      val code = readFile(f)
      try {
        parse(code, TSOptions)
      } catch {
        case err: ErrorHandler.Error =>
          throw FileError(f.getPath, err)
      }
    }
  }

}
