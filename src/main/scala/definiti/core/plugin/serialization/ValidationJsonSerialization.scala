package definiti.core.plugin.serialization

import definiti.common.validation
import definiti.common.validation.{ASTError, Invalid, SimpleError}
import spray.json._

private[core] trait ValidationJsonSerialization {
  self: JsonSerialization =>

  def invalidFromJson(value: String): Invalid = {
    validation.Invalid(seqErrorReader.read(value.parseJson))
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
  private lazy val errorFormat: JsonFormat[validation.Error] = new JsonFormat[validation.Error] {
    override def write(error: validation.Error): JsValue = error match {
      case astError: ASTError => astErrorMessageFormat.write(astError)
      case simpleError: SimpleError => simpleErrorMessageFormat.write(simpleError)
    }

    override def read(json: JsValue): validation.Error = {
      json match {
        case _: JsObject => astErrorMessageFormat.read(json)
        case _: JsString => simpleErrorMessageFormat.read(json)
        case _ => deserializationError(s"Object or String expected, got: ${json}")
      }
    }
  }
  private lazy val seqErrorReader: JsonFormat[Seq[validation.Error]] = seqFormat[validation.Error](errorFormat)
}
