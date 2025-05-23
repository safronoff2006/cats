import sbt.Keys.{name, organization}

// The simplest possible sbt build file is just one line:

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(

      scalaVersion := "2.13.14",
      organization := "safronoff2006",
      version := "1.0",
      libraryDependencies ++= Seq(
        "org.scala-lang.modules" %% "scala-parser-combinators" % "2.4.0",
        "org.typelevel" %% "cats-core" % "2.12.0",
        "org.typelevel" %% "cats-kernel" % "2.12.0",
        "org.typelevel" %% "cats-laws" % "2.11.0",
        "org.typelevel" %% "cats-free" % "2.12.0",
        "org.typelevel" %% "cats-testkit" % "2.12.0",
        "org.typelevel" %% "cats-effect" % "3.5.5",
        "org.typelevel" %% "cats-mtl" % "1.4.0",




      ),


    )),
    scalacOptions ++= Seq("-Xfatal-warnings",  "-Wnonunit-statement"),
    name := "Monada"

  )

addCompilerPlugin("org.typelevel" % "kind-projector" % "0.13.3" cross CrossVersion.full)
addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")


// lazy val root = (project in file(".")).
//   settings(
//     inThisBuild(List(
//       organization := "ch.epfl.scala",
//       scalaVersion := "2.13.12"
//     )),
//     name := "hello-world"
//   )

// To learn more about multi-project builds, head over to the official sbt
// documentation at http://www.scala-sbt.org/documentation.html
