package definiti.core.plugin.serialization

import definiti.core.{ASTError, Error, Invalid, SimpleError}
import spray.json._

trait ValidationJsonSerialization {
  self: JsonSerialization =>

  def invalidFromJson(value: String): Invalid = {
    Invalid(seqErrorReader.read(value.parseJson))
  }

  import spray.json.DefaultJsonProtocol._

  private lazy val astErrorMessageFormat: JsonFormat[ASTError] = jsonFormat2(ASTError.apply)
  private lazy val simpleErrorMessageFormat: JsonFormat[SimpleError] = new JsonFormat[SimpleError] {
    override def read(json: JsValue): SimpleError = json match {
      case JsString(value) => SimpleError(value)
      case _ => deserializationError(s"Expected string, got: ${json}")
    }

    override def write(obj: SimpleError): JsValue = JsString(obj.message)
  }
  private lazy val errorFormat: JsonFormat[Error] = new JsonFormat[Error] {
    override def write(error: Error): JsValue = error match {
      case astError: ASTError => astErrorMessageFormat.write(astError)
      case simpleError: SimpleError => simpleErrorMessageFormat.write(simpleError)
    }

    override def read(json: JsValue): Error = {
      json match {
        case _: JsObject => astErrorMessageFormat.read(json)
        case _: JsString => simpleErrorMessageFormat.read(json)
        case _ => deserializationError(s"Object or String expected, got: ${json}")
      }
    }
  }
  private lazy val seqErrorReader: JsonFormat[Seq[Error]] = seqFormat[Error](errorFormat)
}
