import sbt.Keys.organization
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





      ),
      name := "Monoid"

    )),
    scalacOptions ++= Seq("-Xfatal-warnings")

  )



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
