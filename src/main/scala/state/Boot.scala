package state

object Boot extends App {
  val parts = SyntaxProcessor.processString(Sample.testString)
  parts.foreach(println)
}