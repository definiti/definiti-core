package definiti.core.plugin.serialization

import definiti.core.Configuration
import spray.json._

private[core] class JsonSerialization(val config: Configuration)
  extends GeneratorSerialization
    with LibrarySerialization
    with RootJsonSerialization
    with ValidationJsonSerialization {

  def sealedTraitFormat[A](formats: Format[_ <: A]*): JsonFormat[A] = new JsonFormat[A] {
    override def write(obj: A): JsValue = {
      formats.flatMap(_.writeOpt(obj)).headOption match {
        case Some(value) => value
        case None => serializationError(s"Unknown format for type ${obj.getClass}")
      }
    }

    override def read(json: JsValue): A = {
      formats.flatMap(_.readOpt(json)).headOption match {
        case Some(value) => value
        case None => deserializationError(s"Could not read sealed trait from ${json}")
      }
    }
  }

  def enumerationFormat[A <: Enumeration](enumeration: A): JsonFormat[A#Value] = new JsonFormat[A#Value] {
    override def write(obj: A#Value): JsValue = {
      JsString(obj.toString)
    }

    override def read(json: JsValue): A#Value = {
      json match {
        case JsString(txt) => enumeration.withName(txt)
        case other => deserializationError(s"Expected ${enumeration}, got: ${other}")
      }
    }
  }
}

case class Format[A](typ: String, clazz: Class[A])(implicit jsonFormat: JsonFormat[A]) {
  def writeOpt[B >: A](value: B): Option[JsValue] = {
    if (value.getClass == clazz) {
      val jsValue = jsonFormat.write(value.asInstanceOf[A]) match {
        case JsObject(fields) => JsObject(fields ++ Map("type" -> JsString(typ)))
        case other => other
      }
      Some(jsValue)
    } else {
      None
    }
  }

  def readOpt(json: JsValue): Option[A] = {
    json match {
      case JsObject(fields) if fields.get("type").contains(JsString(typ)) => Some(jsonFormat.read(json))
      case _ => None
    }
  }
}