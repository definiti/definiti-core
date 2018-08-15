import sbt.url

organization := "io.github.definiti"

name := "core"

scalaVersion := "2.12.6"

resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

libraryDependencies += "io.spray" %%  "spray-json" % "1.3.4"
libraryDependencies += "org.antlr" % "antlr4" % "4.7.1"
libraryDependencies += "com.typesafe" % "config" % "1.3.3"
libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.9.0"
libraryDependencies += "org.slf4j" % "slf4j-simple" % "1.7.25"
libraryDependencies += "org.typelevel" %% "cats-core" % "1.2.0"
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.1"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.0" % "test"
libraryDependencies += "io.github.definiti" % "api" % "0.3.1-SNAPSHOT" % "test"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-language:implicitConversions", "-feature")

lazy val antlrDefiniti = TaskKey[Unit]("antlrDefiniti", "Build Antlr Definiti files")
lazy val antlrCore = TaskKey[Unit]("antlrCore", "Build Antlr core definition files")
lazy val antlr = TaskKey[Unit]("antlr", "Build Antlr files")

lazy val classpathSeparator =
  if (sys.props("os.name").toLowerCase.contains("windows")) ";"
  else ":"

antlr := {
  import scala.sys.process._

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

compile in Compile := (compile in Compile).dependsOn(antlr).value

releasePublishArtifactsAction := PgpKeys.publishSigned.value

pomIncludeRepository := { _ => false }

licenses := Seq("MIT" -> url("http://opensource.org/licenses/MIT"))

homepage := Some(url("https://definiti.gitbook.io/definiti"))

scmInfo := Some(
  ScmInfo(
    url("https://github.com/definiti/definiti-core"),
    "scm:git@github.com:definiti/definiti-core.git"
  )
)

developers := List(
  Developer(
    id = "grizio",
    name = "Gaëtan Rizio",
    email = "gaetan@rizio.fr",
    url = url("https://github.com/grizio")
  )
)

publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}