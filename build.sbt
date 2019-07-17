name := "glass"

version := "0.1"

scalaVersion in ThisBuild := "2.12.8"
crossScalaVersions in ThisBuild := List("2.11.12", "2.12.8")

scalacOptions in ThisBuild ++= Vector(
  "-language:existentials", // Existential types (besides wildcard types) can be written and inferred
  "-language:experimental.macros", // Allow macro definition (besides implementation and application)
  "-language:higherKinds", // Allow higher-kinded types
  "-language:implicitConversions", // Allow definition of implicit functions called views
  "-Yno-adapted-args", // Do not adapt an argument list (either by inserting () or creating a tuple) to match the receiver.
  "-Ypartial-unification", // Enable partial unification in type constructor inference
  "-Ywarn-nullary-unit", // Warn when nullary methods return Unit.
  "-Ywarn-value-discard" // Warn when non-Unit expression results are unused.
)

libraryDependencies in ThisBuild += compilerPlugin("org.typelevel" %% "kind-projector" % "0.10.3")
libraryDependencies in ThisBuild += compilerPlugin("com.olegpy" %% "better-monadic-for" % "0.2.4")

lazy val core = project

lazy val interop = project dependsOn core

lazy val glass = project in file(".") aggregate(core, interop)