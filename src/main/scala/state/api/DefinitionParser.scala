package state.api

import state.{AttributeDefinition, ClassDefinition, MethodDefinition, NativeMethodDefinition, ParameterDefinition, NativeClassDefinition}

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

sealed trait SyntaxToken

case object EndOfLine extends SyntaxToken

case object Space extends SyntaxToken

case object OpenBrace extends SyntaxToken

case object CloseBrace extends SyntaxToken

case object OpenParenthesis extends SyntaxToken

case object CloseParenthesis extends SyntaxToken

case class SymbolString(content: String) extends SyntaxToken

case object Colon extends SyntaxToken

case object Comma extends SyntaxToken

case class LineComment(content: String) extends SyntaxToken

case class BlockComment(content: String) extends SyntaxToken

case class Word(content: String) extends SyntaxToken

case object TypeKeyword extends SyntaxToken

case object Void extends SyntaxToken

case object Unknown extends SyntaxToken

object DefinitionParser {
  type Syntax = Seq[SyntaxToken]

  def parse(source: String): Seq[ClassDefinition] = {
    val syntax: Syntax = extractSyntax(source)

    @tailrec
    def processTopLevel(acc: Seq[ClassDefinition], remaining: Syntax): Seq[ClassDefinition] = remaining match {
      case Nil => acc
      case (EndOfLine | LineComment(_) | BlockComment(_)) :: tail => processTopLevel(acc, tail)
      case TypeKeyword :: tail => processType(tail) match {
        case (classDefinition, remainingTail) => processTopLevel(acc :+ classDefinition, remainingTail)
      }
      case token :: _ => throw new RuntimeException("Expected type keyword, got: " + token)
    }

    @tailrec
    def processType(remaining: Syntax): (ClassDefinition, Syntax) = remaining match {
      case Nil => throw new RuntimeException("Expected type name, got EOF")
      case (EndOfLine | LineComment(_) | BlockComment(_)) :: tail => processType(tail)
      case Word(typeName) :: tail => processTypeBrace(typeName, tail)
      case token :: _ => throw new RuntimeException("Expected type name, got: " + token)
    }

    @tailrec
    def processTypeBrace(typeName: String, remaining: Syntax): (ClassDefinition, Syntax) = remaining match {
      case Nil => throw new RuntimeException("Expected opening brace, got: EOF")
      case (EndOfLine | LineComment(_) | BlockComment(_)) :: tail => processTypeBrace(typeName, tail)
      case OpenBrace :: tail => processTypeContent(typeName, Nil, Nil, tail)
      case token :: _ => throw new RuntimeException("Expected {, got: " + token)
    }

    @tailrec
    def processTypeContent(typeName: String, attributesAcc: Seq[AttributeDefinition], methodsAcc: Seq[NativeMethodDefinition], remaining: Syntax): (ClassDefinition, Syntax) = remaining match {
      case Nil => throw new RuntimeException("Expected method or attribute definition, got: EOF")
      case (EndOfLine | LineComment(_) | BlockComment(_)) :: tail => processTypeContent(typeName, attributesAcc, methodsAcc, tail)
      case CloseBrace :: tail => (NativeClassDefinition(typeName, attributesAcc, methodsAcc), tail)
      case Word(attributeOrMethodName) :: tail =>
        processAttributeOrMethod(attributeOrMethodName, tail) match {
          case (Left(attribute), remainingTail) =>
            processTypeContent(typeName, attributesAcc :+ attribute, methodsAcc, remainingTail)
          case (Right(method), remainingTail) =>
            processTypeContent(typeName, attributesAcc, methodsAcc :+ method, remainingTail)
        }
      case token :: _ => throw new RuntimeException("Expected attribute or method name, got: " + token)
    }

    @tailrec
    def processAttributeOrMethod(attributeOrMethodName: String, remaining: Syntax): (Either[AttributeDefinition, NativeMethodDefinition], Syntax) = remaining match {
      case Nil => throw new RuntimeException("Expected ( or :, got: EOF")
      case (EndOfLine | LineComment(_) | BlockComment(_)) :: tail => processAttributeOrMethod(attributeOrMethodName, tail)
      case Colon :: tail =>
        processAttributeType(attributeOrMethodName, tail) match {
          case (attribute, remainingTail@((EndOfLine | CloseBrace) :: _)) =>
            (Left(attribute), remainingTail)
          case (_, token :: _) => throw new RuntimeException("Expected EOL or }, got: " + token)
        }
      case OpenParenthesis :: tail =>
        processMethodParameters(Nil, tail) match {
          case (parameters, tailBeforeReturnType) => processMethodReturnColon(tailBeforeReturnType) match {
            case (returnType, remainingTail@((EndOfLine | CloseBrace) :: _)) =>
              (Right(NativeMethodDefinition(attributeOrMethodName, parameters, returnType)), remainingTail)
            case (_, token :: _) => throw new RuntimeException("Expected EOL or }, got: " + token)
          }
        }
      case token :: _ => throw new RuntimeException("Expected attribute or method name, got: " + token)
    }

    @tailrec
    def processMethodParameters(parameters: Seq[ParameterDefinition], remaining: Syntax): (Seq[ParameterDefinition], Syntax) = remaining match {
      case Nil => throw new RuntimeException("Expected parameter name, got: EOF")
      case (EndOfLine | LineComment(_) | BlockComment(_)) :: tail => processMethodParameters(parameters, tail)
      case Word(parameterName) :: tail => processMethodParameterColon(parameterName, tail) match {
        case (parameter, remainingTail) => processMethodParameters(parameters :+ parameter, remainingTail)
      }
      case CloseParenthesis :: tail => (parameters, tail)
      case token :: _ => throw new RuntimeException("Expected parameter name, got: " + token)
    }

    @tailrec
    def processMethodParameterColon(parameterName: String, remaining: Syntax): (ParameterDefinition, Syntax) = remaining match {
      case Nil => throw new RuntimeException("Expected :, got: EOF")
      case (EndOfLine | LineComment(_) | BlockComment(_)) :: tail => processMethodParameterColon(parameterName, tail)
      case Colon :: tail => processMethodParameterType(parameterName, tail)
      case token :: _ => throw new RuntimeException("Expected :, got: " + token)
    }

    @tailrec
    def processMethodParameterType(parameterName: String, remaining: Syntax): (ParameterDefinition, Syntax) = remaining match {
      case Nil => throw new RuntimeException("Expected parameter type, got: EOF")
      case (EndOfLine | LineComment(_) | BlockComment(_)) :: tail => processMethodParameterType(parameterName, tail)
      case Word(parameterType) :: tail => (ParameterDefinition(parameterName, parameterType), tail)
      case token :: _ => throw new RuntimeException("Expected parameter name, got: " + token)
    }

    @tailrec
    def processMethodReturnColon(remaining: Syntax): (String, Syntax) = remaining match {
      case Nil => throw new RuntimeException("Expected parameter type, got: EOF")
      case (EndOfLine | LineComment(_) | BlockComment(_)) :: tail => processMethodReturnColon(tail)
      case Colon :: tail => processMethodReturnType(tail)
      case token :: _ => throw new RuntimeException("Expected parameter name, got: " + token)
    }

    @tailrec
    def processMethodReturnType(remaining: Syntax): (String, Syntax) = remaining match {
      case Nil => throw new RuntimeException("Expected parameter type, got: EOF")
      case (EndOfLine | LineComment(_) | BlockComment(_)) :: tail => processMethodReturnType(tail)
      case Word(returnType) :: tail => (returnType, tail)
      case token :: _ => throw new RuntimeException("Expected parameter name, got: " + token)
    }

    @tailrec
    def processAttributeType(attributeName: String, remaining: Syntax): (AttributeDefinition, Syntax) = remaining match {
      case Nil => throw new RuntimeException("Expected parameter type, got: EOF")
      case (EndOfLine | LineComment(_) | BlockComment(_)) :: tail => processAttributeType(attributeName, tail)
      case Word(attributeType) :: tail => (AttributeDefinition(attributeName, attributeType), tail)
      case token :: _ => throw new RuntimeException("Expected parameter name, got: " + token)
    }

    processTopLevel(Nil, syntax)
  }

  private def extractSyntax(source: String): Syntax = {
    val parts = ListBuffer[SyntaxToken]()

    var acc: SyntaxToken = Void
    source.foreach { char =>
      (char, acc) match {
        case ('\r', _) => // Ignore
        case ('*', SymbolString("/")) =>
          acc = BlockComment("")
        case ('/', BlockComment(content)) if content.endsWith("*") =>
          parts.append(BlockComment(content.substring(0, content.length - 1)))
          acc = Void
        case (c, BlockComment(content)) =>
          acc = BlockComment(content + c)
        case ('\n', EndOfLine) => // Do nothing
        case ('\n', _) =>
          parts.append(acc)
          acc = EndOfLine
        case ('/', SymbolString("/")) =>
          acc = LineComment("")
        case (c, LineComment(content)) =>
          acc = LineComment(content + c)
        case (c, Space) if c.isSpaceChar => // Do nothing
        case (c, _) if c.isSpaceChar =>
          parts.append(acc)
          acc = Space
        case ('{', _) =>
          parts.append(acc)
          acc = OpenBrace
        case ('}', _) =>
          parts.append(acc)
          acc = CloseBrace
        case ('(', _) =>
          parts.append(acc)
          acc = OpenParenthesis
        case (')', _) =>
          parts.append(acc)
          acc = CloseParenthesis
        case (':', _) =>
          parts.append(acc)
          acc = Colon
        case (',', _) =>
          parts.append(acc)
          acc = Comma
        case (('/' | '*'), SymbolString(content)) =>
          acc = SymbolString(content + char)
        case (('/' | '*'), _) =>
          parts.append(acc)
          acc = SymbolString(char.toString)
        case (c, Word(content)) if c.isLetterOrDigit =>
          acc = Word(content + char)
        case (c, _) if c.isLetterOrDigit =>
          parts.append(acc)
          acc = Word(char.toString)
        case _ =>
          acc = Unknown
      }
    }

    parts
      .view
      .filter(_ != Void)
      .filter(_ != Space)
      .map {
        case Word("type") => TypeKeyword
        case other => other
      }
      .foldLeft(List[SyntaxToken]()) { case (resultAcc, token) =>
        if (token == EndOfLine && resultAcc.lastOption.contains(EndOfLine)) {
          resultAcc
        } else {
          resultAcc :+ token
        }
      }
  }
}
