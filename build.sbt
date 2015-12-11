lazy val scalaV = "2.11.7"

lazy val root = (project in file(".")).
  settings(
    libraryDependencies ++= Seq(
      "com.chuusai" %% "shapeless" % "2.2.5",
      "org.monifu" %% "monifu" % "1.0-RC4",
      "org.spire-math" %% "cats" % "0.3.0",
      "org.spire-math" %% "spire" % "0.11.0",

      "org.scalatest" %% "scalatest" % "2.2.4" % "test",
      "org.scalacheck" %% "scalacheck" % "1.12.5" % "test",
      "org.typelevel" %% "shapeless-scalacheck" % "0.4"
    ),
    scalaVersion := scalaV
  )
