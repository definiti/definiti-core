organization := "definiti"

name := "core"

version := "0.0.0"

scalaVersion := "2.12.1"

libraryDependencies += "io.spray" %%  "spray-json" % "1.3.3"
libraryDependencies += "org.antlr" % "antlr4" % "4.7"

// For developmen purposes
libraryDependencies += "com.googlecode.kiama" %% "kiama" % "1.8.0"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.5" % "test"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-language:implicitConversions", "-feature")

lazy val antlrDefiniti = TaskKey[Unit]("antlrDefiniti", "Build Antlr Definiti files")
lazy val antlrCore = TaskKey[Unit]("antlrCore", "Build Antlr core definition files")
lazy val antlr = TaskKey[Unit]("antlr", "Build Antlr files")

lazy val classpathSeparator =
  if (sys.props("os.name").toLowerCase.contains("windows")) ";"
  else ":"

antlrDefiniti := {
  val log = streams.value.log
  val classpath = (dependencyClasspath in Compile).value.files.mkString(classpathSeparator)
  val mainClass = "org.antlr.v4.Tool"
  val destination = "src/main/java/definiti/core/parser/antlr"
  val packageName = "definiti.core.parser.antlr"
  val source = "src/main/antlr/Definiti.g4"

  val command = s"""java -cp "$classpath" $mainClass -o $destination -package $packageName $source"""

  log.info("Building antlr Definiti files")
  command.!
}

antlrCore := {
  val log = streams.value.log
  val classpath = (dependencyClasspath in Compile).value.files.mkString(classpathSeparator)
  val mainClass = "org.antlr.v4.Tool"
  val destination = "src/main/java/definiti/core/parser/antlr"
  val packageName = "definiti.core.parser.antlr"
  val source = "src/main/antlr/CoreDefinition.g4"

  val command = s"""java -cp "$classpath" $mainClass -o $destination -package $packageName $source"""

  log.info("Building antlr core definition files")
  command.!
}

antlr := {}

antlr <<= antlr.dependsOn(antlrDefiniti, antlrCore)

compile in Compile <<= (compile in Compile).dependsOn(antlr)