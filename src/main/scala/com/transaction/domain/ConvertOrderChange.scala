package transaction.domain

import play.api.libs.json._
import play.api.libs.functional.syntax._

object ConvertOrderChange {
  implicit val impl = Json.format[ConvertOrderChange]
}

case class ConvertOrderChange(
  account: String,
  taker_pays: JsValue,
  taker_gets: JsValue,
  sell: Boolean,
  sequence: BigDecimal,
  status: String,
  quality: String,
  expiration: String) {
  def toJson = Json.toJson(this)
}
