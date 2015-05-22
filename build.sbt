name := "3dpuzzle"

/*
2.0 : add the 2d puzzle GUI
 */
version := "2.0-devel"

scalaVersion := "2.11.6"

libraryDependencies += "org.ow2.sat4j" % "org.ow2.sat4j.core" % "2.3.5"

libraryDependencies += "org.specs2" %% "specs2" % "2.3.12"

libraryDependencies += "com.jsuereth" %% "scala-arm" % "1.4"

libraryDependencies += "com.github.scopt" %% "scopt" % "3.2.0"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.2"

//json parsing
libraryDependencies += "io.spray" % "spray-json_2.11" % "1.3.2"

//for the gui
libraryDependencies += "io.reactivex" % "rxscala_2.11" % "0.24.1"

libraryDependencies += "org.scala-lang.modules" % "scala-swing_2.11" % "2.0.0-M2"

libraryDependencies += "com.miglayout" % "miglayout-swing" % "5.0"

