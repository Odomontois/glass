libraryDependencies += "com.github.julien-truffaut" %% "monocle-core"  % "1.5.1-cats"
libraryDependencies += "com.github.julien-truffaut" %% "monocle-macro" % "1.5.1-cats" % Test
libraryDependencies += { scalaOrganization.value % "scala-reflect" % scalaVersion.value % Test }
