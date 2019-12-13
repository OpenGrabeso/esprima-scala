package com.github.opengrabeso.esprima

import com.github.opengrabeso.esprima.Esprima._
import org.scalatest.FlatSpec

class DTSTests extends FlatSpec with TestInputs {
  object DTSOptions extends Parser.Options {
    range = true
    attachComment = true
    tolerant = true
  }

  behavior of "Parsing Three.js d.ts"

  it should "process Box2" in {
    val input = fromResource("/threejs/d.ts/Box2.d.ts")
    pendingUntilFixed {
      val tree = parse(input, DTSOptions)
      assert(tree.body.nonEmpty)
    }
  }

  ignore should "process Quaternion" in {
    val input = fromResource("/threejs/d.ts/Quaternion.d.ts")
    pendingUntilFixed {
      val tree = parse(input, DTSOptions)
      assert(tree.body.nonEmpty)
    }
  }

  ignore should "process Object3D" in {
    val input = fromResource("/threejs/d.ts/Object3D.d.ts")
    pendingUntilFixed {
      val tree = parse(input, DTSOptions)
      assert(tree.body.nonEmpty)
    }
  }

}
