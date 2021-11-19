name := "stringy-dsl"

version := "0.1"

scalaVersion := "2.13.7"

libraryDependencies ++= List(
  "org.typelevel"     %% "cats-parse"               % "0.3.5",
  "io.higherkindness" %% "droste-core"              % "0.9.0-M3",
  "org.scalatest"     %% "scalatest-wordspec"       % "3.2.10"   % Test,
  "org.scalatest"     %% "scalatest-shouldmatchers" % "3.2.10"   % Test,
  "org.scalacheck"    %% "scalacheck"               % "1.15.4"   % Test,
  "org.scalatestplus" %% "scalacheck-1-15"          % "3.2.10.0" % Test
)

addCompilerPlugin("org.typelevel" % "kind-projector" % "0.13.2" cross CrossVersion.full)

scalacOptions ++= List(
  "-Ymacro-annotations",
  "-deprecation"
)
