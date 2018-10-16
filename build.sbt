name := "3dpuzzle"

version := "2.0"

scalaVersion := "2.12.6"

mainClass in assembly := Some("puzzle2d.gui.Main")

libraryDependencies += "org.specs2" %% "specs2-core" % "4.3.4" % Test

libraryDependencies += "org.specs2" %% "specs2-matcher-extra" % "4.3.4" % Test

libraryDependencies += "com.jsuereth" %% "scala-arm" % "2.0"

libraryDependencies += "com.github.scopt" %% "scopt" % "3.7.0"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.1"

//for the gui
libraryDependencies += "io.reactivex" %% "rxscala" % "0.26.5"

libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "2.0.3"

//json parsing
libraryDependencies += "io.spray" %% "spray-json" % "1.3.4"

// Java dependencies below

libraryDependencies += "org.ow2.sat4j" % "org.ow2.sat4j.core" % "2.3.5"

//writing to svg
libraryDependencies += "org.jfree" % "jfreesvg" % "3.0"

libraryDependencies += "com.miglayout" % "miglayout-swing" % "5.0"

