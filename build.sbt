val dottyVersion = dottyLatestNightlyBuild.getOrElse("0.5.0-RC1")

lazy val root = (project in file("."))
  .settings(
    inThisBuild(List(
      organization := "org.halcat",
      scalaVersion := dottyVersion,
      version := "0.1"
    )),
    name := "kits-eff-dotty"
  )

lazy val core = project in file("core")

lazy val bench = (project in file("bench"))
  .dependsOn(core)
  .enablePlugins(JmhPlugin)
