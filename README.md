# esprima-scala
Port of Esprima JS parser to Scala

Converted from https://github.com/jquery/esprima with help of https://github.com/OndrejSpanel/ScalaFromJS

The main purpose is to be used in https://github.com/OndrejSpanel/ScalaFromJS so that the tool can be used on JVM.

The tool is currently in alpha.

No release version exists so far.

To use the snapshot version in your build use:

```
resolvers += Resolver.sonatypeRepo("snapshots")

libraryDependencies += "com.github.opengrabeso" %% "esprimascala" % "0.0.1-SNAPSHOT"
```
