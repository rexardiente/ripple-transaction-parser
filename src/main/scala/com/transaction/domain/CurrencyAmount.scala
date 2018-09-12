package transaction.domain

import play.api.libs.json._

object CurrencyAmount {
  implicit val implCurrencyAmount = Json.format[CurrencyAmount]
}

case class CurrencyAmount(currency: String, counterparty: Option[String], value: BigDecimal) {
  def toJson = Json.toJson(this)
}
