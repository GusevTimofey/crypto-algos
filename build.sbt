name := "crypto-algos"

version := "0.1"

scalaVersion := "2.13.2"

libraryDependencies ++= Seq(
  "com.google.guava" % "guava"       % "28.2-jre",
  "org.typelevel"    %% "mouse"      % "0.24",
  "org.typelevel"    %% "simulacrum" % "1.0.0",
  "org.typelevel"    %% "cats-core"  % "2.1.1",
  "io.estatico"      %% "newtype"    % "0.4.3"
)

scalacOptions in ThisBuild ++= Seq(
  "-language:_",
  "-Ymacro-annotations",
  "-language:postfixOps",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-Xfatal-warnings",
  "-unchecked",
  "-feature",
  "-deprecation"
)

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full)
