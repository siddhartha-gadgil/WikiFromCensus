name := "WikiFromCensus"

version := "0.1"

scalaVersion := "2.11.7"

libraryDependencies  ++= Seq(
  "com.lihaoyi" % "ammonite-repl" % "0.4.8" % "test" cross CrossVersion.full
  )

resolvers ++= Seq(
  // other resolvers here
  // if you want to use snapshot builds (currently 0.12-SNAPSHOT), use this.
  "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/",
  "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"
  )

initialCommands in (Test, console) := """ammonite.repl.Repl.run("")"""
