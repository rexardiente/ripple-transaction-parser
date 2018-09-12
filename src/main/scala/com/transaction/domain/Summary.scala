package transaction.domain

import play.api.libs.json._

object Summary {
  implicit val implSummary = Json.format[Summary]
}

case class Summary(
  status: String,
  channelId: String,
  source: String,
  destination: String,
  channelAmountDrops: String,
  channelBalanceDrops: String,
  channelAmountChangeDrops: String,
  channelBalanceChangeDrops: String,
  previousTxnId: String) {
  def toJson = Json.toJson(this)
}
