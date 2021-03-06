lazy val settings = Seq(
  name := "crypto-algos",
  scalacOptions ++= options,
  version := "0.1",
  resolvers += Resolver.sonatypeRepo("public"),
  resolvers += Resolver.sonatypeRepo("snapshots"),
  scalaVersion := "2.13.1"
)

lazy val fs2Version         = "2.1.0"
lazy val catsVersion        = "2.1.1"
lazy val catsEffectsVersion = "2.0.0"
lazy val http4sVersion      = "0.21.0"
lazy val circeVersion       = "0.12.3"
lazy val redisVersion       = "0.9.3"

lazy val cats: Seq[ModuleID] = Seq(
  "org.typelevel" %% "cats-core"   % catsVersion,
  "org.typelevel" %% "cats-effect" % catsEffectsVersion,
  "co.fs2"        %% "fs2-core"    % fs2Version,
  "co.fs2"        %% "fs2-io"      % fs2Version
)

lazy val http4s: Seq[ModuleID] = Seq(
  "org.http4s" %% "http4s-dsl"          % http4sVersion,
  "org.http4s" %% "http4s-blaze-server" % http4sVersion,
  "org.http4s" %% "http4s-blaze-client" % http4sVersion,
  "org.http4s" %% "http4s-circe"        % http4sVersion
)

lazy val circe: Seq[ModuleID] = Seq(
  "io.circe" %% "circe-core"    % circeVersion,
  "io.circe" %% "circe-generic" % circeVersion,
  "io.circe" %% "circe-parser"  % circeVersion,
  "io.circe" %% "circe-refined" % circeVersion
)

lazy val dependencies = Seq(
  "com.google.guava" % "guava"       % "28.2-jre",
  "org.typelevel"    %% "mouse"      % "0.24",
  "io.monix"         %% "monix"      % "3.1.0",
  "org.typelevel"    %% "simulacrum" % "1.0.0",
  "io.estatico"      %% "newtype"    % "0.4.3",
  "org.scalatest"    %% "scalatest"  % "3.1.0" % Test,
  compilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full)
) ++ cats

lazy val options = Seq(
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

lazy val root = project
  .in(file("."))
  .withId("crypto-algos")
  .settings(settings)
  .settings(moduleName := "crypto-algos", name := "crypto-algos")
  .aggregate(algorithms, benchmarks, apps)

lazy val algorithms = project
  .in(file(s"modules/algorithms"))
  .settings(moduleName := "algorithms", name := "algorithms")
  .withId("algorithms")
  .settings(settings)
  .settings(libraryDependencies ++= dependencies)

lazy val benchmarks = project
  .in(file(s"modules/benchmarks"))
  .settings(moduleName := "benchmarks", name := "benchmarks")
  .withId("benchmarks")
  .settings(settings)
  .settings(libraryDependencies ++= dependencies ++ Seq("org.openjdk.jmh" % "jmh-generator-annprocess" % "1.21"))
  .enablePlugins(JmhPlugin)
  .dependsOn(algorithms % "test->test")

lazy val apps = project
  .in(file(s"modules/apps"))
  .settings(moduleName := "apps", name := "apps")
  .withId("apps")
  .settings(settings)
  .settings(
    libraryDependencies ++= dependencies ++ http4s ++ Seq(
      "org.rocksdb"       % "rocksdbjni"          % "6.4.6",
      "ch.qos.logback"    % "logback-classic"     % "1.2.3",
      "io.chrisdavenport" %% "log4cats-slf4j"     % "0.4.0-M2",
      "org.bouncycastle"  % "bcprov-jdk15on"      % "1.65",
      "dev.profunktor"    %% "redis4cats-effects" % redisVersion,
      "dev.profunktor"    %% "redis4cats-streams" % redisVersion,
      compilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")
    ) ++ circe
  )
  .dependsOn(algorithms)
