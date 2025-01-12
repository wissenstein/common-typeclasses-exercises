scalaVersion := "3.6.2"
ThisBuild / organization := "com.example"

lazy val hello = (project in file("."))
  .settings(
    name := "Common Typeclasses Exercises"
  )

libraryDependencies += "org.typelevel" %% "cats-core" % "2.12.0"
libraryDependencies += "org.typelevel" %% "cats-laws" % "2.12.0"
libraryDependencies += "org.typelevel" %% "discipline-core" % "1.7.0" % Test
libraryDependencies += "org.typelevel" %% "discipline-scalatest" % "2.3.0" % Test
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19"
libraryDependencies += "org.typelevel" %% "cats-effect" % "3.5.7"
