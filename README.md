# esprima-scala
Port of Esprima JS parser to Scala

TypeScript is also partially supported, to the extent complete Esprima sources and most d.ts files can be parsed.

Converted from https://github.com/jquery/esprima with help of https://github.com/OndrejSpanel/ScalaFromJS

The main purpose is to be used in https://github.com/OndrejSpanel/ScalaFromJS so that the tool can be used on JVM.

The tool is currently in alpha.

No release version exists so far.

To use the snapshot version in your build use:

```
resolvers += Resolver.sonatypeRepo("snapshots")

libraryDependencies += "com.github.opengrabeso" %% "esprimascala" % "0.1.3-SNAPSHOT"
```


### Conversion workflow

- switch to branch `untouched`
- place current Esprima TypeScript sources in `/ts/src`
- use `ScalaFromJS` to convert the files, result will be placed in `/ts/scala`
 - `java -jar ScalaFromJS.jar ts/esprima-convert.ts ts/scala/esprima-convert.scala`
- switch to branch `port`
- merge `untouched` to `port`
- cleanup as necessary
 - once it compiles and passes tests,  commit

...
- merge into master, publish
 
