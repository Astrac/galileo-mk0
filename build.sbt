import com.lihaoyi.workbench.Plugin._

lazy val scalaV = "2.11.7"

lazy val core = (crossProject in file("core"))
  .settings(
    libraryDependencies ++= Seq(
      "com.chuusai" %%% "shapeless" % "2.2.5",
      "org.monifu" %%% "monifu" % "1.0-RC4",
      "org.spire-math" %%% "cats" % "0.3.0",
      "org.spire-math" %%% "spire" % "0.11.0"
    ),
    scalaVersion := scalaV
  )
  .jvmSettings(
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "2.2.4" % "test",
      "org.scalacheck" %% "scalacheck" % "1.12.5" % "test",
      "org.typelevel" %% "shapeless-scalacheck" % "0.4" % "test"
    )
  )

lazy val playground = crossProject.in(file("playground"))
  .settings(
    name := "playground",
    scalaVersion := "2.11.7",
    resolvers += sbt.Resolver.bintrayRepo("denigma", "denigma-releases")
  )
  .jvmSettings()
  .jsSettings(workbenchSettings ++ Seq(
    bootSnippet := "astrac.galileo.playground.Demo.main()",
    updateBrowsers <<= updateBrowsers.triggeredBy(fastOptJS in Compile),
    libraryDependencies ++= Seq(
      "com.github.japgolly.scalacss" %%% "core" % "0.3.1",
      "com.lihaoyi" %%% "scalatags" % "0.5.3",
      "org.denigma" %%% "threejs-facade" % "0.0.71-0.1.5",
      "org.scala-js" %%% "scalajs-dom" % "0.8.2"
    )
  ): _*)

lazy val coreJs = core.js
lazy val coreJvm = core.jvm
lazy val playgroundJs = playground.js.dependsOn(coreJs)
lazy val playgroundJvm = playground.jvm.dependsOn(coreJvm)

lazy val root = (project in file(".")).enablePlugins(ScalaJSPlugin).aggregate(coreJs, coreJvm, playgroundJs, playgroundJvm)
