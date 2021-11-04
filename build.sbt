name := "ParseDSL"

version := "0.1"

scalaVersion := "2.13.7"

libraryDependencies ++= List(
  "com.lihaoyi"       %% "fastparse"          % "2.2.2",
  "org.scalatest"     %% "scalatest-wordspec" % "3.2.10"   % Test,
  "org.scalacheck"    %% "scalacheck"         % "1.15.4"   % Test,
  "org.scalatestplus" %% "scalacheck-1-15"    % "3.2.10.0" % Test
)
