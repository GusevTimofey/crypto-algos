name := "crypto-algos"

version := "0.1"

scalaVersion := "2.13.2"

val fs2Version         = "2.1.0"
val catsVersion        = "2.1.1"
val catsEffectsVersion = "2.0.0"

val cats: Seq[ModuleID] = Seq(
  "org.typelevel" %% "cats-core"   % catsVersion,
  "org.typelevel" %% "cats-effect" % catsEffectsVersion,
  "co.fs2"        %% "fs2-core"    % fs2Version,
  "co.fs2"        %% "fs2-io"      % fs2Version
)

libraryDependencies ++= Seq(
  "com.google.guava" % "guava"       % "28.2-jre",
  "org.typelevel"    %% "mouse"      % "0.24",
  "org.typelevel"    %% "simulacrum" % "1.0.0",
  "io.estatico"      %% "newtype"    % "0.4.3"
) ++ cats

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
enablePlugins(JmhPlugin)
