val scala3Version = "3.0.0"

lazy val deriving = project
  .in(file("deriving"))
  .settings(
    organization := "com.riskified",
    version := "0.1.0",
    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test",
    scalaVersion := scala3Version
  )