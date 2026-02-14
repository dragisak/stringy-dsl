name := "stringy-dsl"

version := "0.1"

scalaVersion := "2.13.18"

libraryDependencies ++= List(
  "org.typelevel"     %% "cats-parse"               % "1.1.0",
  "io.higherkindness" %% "droste-core"              % "0.10.0",
  "org.scalatest"     %% "scalatest-wordspec"       % "3.2.19"   % Test,
  "org.scalatest"     %% "scalatest-shouldmatchers" % "3.2.19"   % Test,
  "org.scalacheck"    %% "scalacheck"               % "1.19.0"   % Test,
  "org.scalatestplus" %% "scalacheck-1-19"          % "3.2.19.0" % Test
)

addCompilerPlugin("org.typelevel" % "kind-projector" % "0.13.4" cross CrossVersion.full)

scalacOptions ++= List(
  "-Ymacro-annotations"
)
