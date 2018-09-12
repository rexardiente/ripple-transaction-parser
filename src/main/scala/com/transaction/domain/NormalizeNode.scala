package transaction.domain

import play.api.libs.json._
import play.api.libs.functional.syntax._

object NormalizeNode {
  implicit val implNormalizeNode = Json.format[NormalizeNode]
}

case class NormalizeNode(
  diffType: String,
  entryType: String,
  ledgerIndex: String,
  newFields: Option[JsValue],
  finalFields: Option[JsValue],
  previousFields: Option[JsValue],
  PreviousTxnID: Option[String]) {
  def toJson() = Json.toJson(this)
}
