lazy val psi = (project in file("."))
  .settings(
    name := "psi",
    version := "0.1.0-BETA0",
    scalaVersion := "3.7.4",

    scalacOptions ++= Seq(
      "-feature",
    ),
  )
