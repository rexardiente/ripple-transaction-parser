package transaction.domain

import play.api.libs.json._
import play.api.libs.functional.syntax._

object TakerJson     { implicit val implTakerJson     = Json.format[TakerJson] }

object CounterParty  { implicit val implCounterParty  = Json.format[CounterParty] }

object Balance       { implicit val implBalance       = Json.format[Balance] }

object Takers {
  import TakerJson._
  import CounterParty._

  implicit val tempResultR1: Reads[Takers] = {
    Json.format[TakerJson] .map(x => x: Takers) or
    Json.format[CounterParty] .map(x => x: Takers)
  }

  implicit val tempResultW = new Writes[Takers] {
    def writes(takers: Takers): JsValue = {
      takers match {
        case m: TakerJson    => Json.toJson(m)
        case m: CounterParty => Json.toJson(m)
        case _               => Json.obj("error" -> "wrong Json")
      }
    }
  }
}
sealed trait Takers  { def toJson = Json.toJson(this) }

case class TakerJson(currency: String, issuer: String, value: String)           extends Takers {
  override def toJson = Json.toJson(this)
}

case class CounterParty(currency: String, counterparty: String, value: String)  extends Takers {
  override def toJson = Json.toJson(this)
}

case class Balance(address: String, balance: CounterParty) {
  def toJson = Json.toJson(this)
}
