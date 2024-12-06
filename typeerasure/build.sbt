


scalaVersion := "2.13.14"


name := "type-erasure"
organization := "safronoff2006"
version := "1.0"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.4.0"
libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.13.14"

scalacOptions ++= Seq(          // use ++= to add to existing options
  "-encoding", "utf8",          // if an option takes an arg, supply it on the same line
  "-feature",                   // then put the next option on a new line for easy editing
  "-language:implicitConversions",
  "-language:existentials",
  "-unchecked",
  "-Werror",
  //"-Xlint",                     // exploit "trailing comma" syntax so you can add an option without editing this line
)                               // for "trailing comma", the closing paren must be on the next line