name := "prj"

version := "1.0"

scalaVersion := "2.12.2"

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect" % "2.12.2",
  "org.scala-lang.modules" %% "scala-xml" %  "1.0.6",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.6",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test",
  "org.scalaz" %% "scalaz-core" % "7.2.13",
  "org.slf4j" % "slf4j-api" % "1.7.25",
  "ch.qos.logback" % "logback-classic" % "1.2.3",
  "com.github.scopt" %% "scopt" % "3.6.0"
)

unmanagedJars in Compile := (file(System.getProperty("java.home")) / ".." / "lib" * "tools.jar").classpath

scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-unchecked",
  "-Xlint",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Ywarn-unused",
  "-Ywarn-unused-import",
  "-Ywarn-value-discard"
)

mainClass in (Compile, run) := Some("phenan.prj.JCompiler")