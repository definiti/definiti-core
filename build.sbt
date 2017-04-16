name := "definiti"

version := "0.0.0"

scalaVersion := "2.12.1"

libraryDependencies += "io.spray" %%  "spray-json" % "1.3.3"
libraryDependencies += "org.antlr" % "antlr4" % "4.7"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-language:implicitConversions", "-feature")

lazy val antlr = TaskKey[Unit]("antlr", "Build Antlr files")

antlr := {
  val log = streams.value.log
  val classpath = (dependencyClasspath in Compile).value.files.mkString(";")
  val mainClass = "org.antlr.v4.Tool"
  val destination = "src/main/java/definiti/parser/antlr"
  val packageName = "definiti.parser.antlr"
  val source = "src/main/antlr/Definiti.g4"

  val command = s"""java -cp "$classpath" $mainClass -o $destination -package $packageName $source"""

  log.info("Building antlr")
  command.!
}

compile in Compile <<= (compile in Compile).dependsOn(antlr)