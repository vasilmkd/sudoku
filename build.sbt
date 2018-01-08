lazy val root = (project in file("."))
  .settings(
    name         := "sudoku",
    scalaVersion := "2.12.4",
    version      := "0.1.0-SNAPSHOT",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.4" % Test
  )
