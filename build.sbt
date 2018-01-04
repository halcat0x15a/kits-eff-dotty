val dottyVersion = "0.5.0-RC1"

lazy val root = (project in file(".")).
  settings(
    name := "kits-eff-dotty",
    version := "0.1",
    scalaVersion := dottyLatestNightlyBuild.getOrElse(dottyVersion)
  )
